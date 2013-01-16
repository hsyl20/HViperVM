module ViperVM.Internals.Memory where

import ViperVM.Buffer
import qualified ViperVM.Buffer as Buffer (getMemory,getBufferImpl)
import ViperVM.Data
import ViperVM.Event
import ViperVM.Internals.Structures
import ViperVM.Platform
import ViperVM.Transfer
import ViperVM.Region
import qualified ViperVM.Region as Region (getMemory)
import Foreign.Ptr
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Event
import Data.Maybe (isJust)
import Data.Traversable (traverse)
import Data.Lens.Lazy
import Data.Word
import Data.Map (Map,(!))
import Data.Set (Set)
import Control.Monad.State
import Control.Concurrent.Chan
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- | Register a data with an initial instance
registerDataInstanceR :: Data -> DataInstance -> R ()
registerDataInstanceR d di = modify (datas ^%= modDatas)
  where
    modDatas = Map.alter f d
    f (Just x) = Just (x ++ [di])
    f Nothing  = Just [di]

-- | Register a new buffer
registerBufferR :: Memory -> Buffer -> R ()
registerBufferR mem buf = modify (memBuffers ^%= modBuffer)
  where
    modBuffer = Map.alter f mem
    f (Just x) = Just (x ++ [buf])
    f Nothing  = Just [buf]

-- | Register data event
registerDataEventR :: Data -> Event () -> R ()
registerDataEventR d ev = modify (dataEvents ^%= modDataEvents)
  where
    modDataEvents = Map.alter f d
    f (Just x) = Just (x ++ [ev])
    f Nothing  = Just [ev]

-- | Unregister a buffer
unregisterBufferR :: Buffer -> R ()
unregisterBufferR buf = modify (memBuffers ^%= modBuffer mem)
  where
    mem = Buffer.getMemory buf
    modBuffer :: Memory -> Map Memory [Buffer] -> Map Memory [Buffer]
    modBuffer m = Map.alter (fmap $ List.delete buf) m

-- | Return memory containing a data instance
getDataInstanceMemory :: DataInstance -> Memory
getDataInstanceMemory (Vector (Region1D buf _ _)) = Buffer.getMemory buf
getDataInstanceMemory _ = undefined

checkTransfer :: Link -> Region -> Region -> Bool
checkTransfer l v1 v2 = lm1 == m1 && lm2 == m2 && checkCompatibleRegions v1 v2
  where
    (lm1,lm2) = getLinkMemories l
    m1 = Region.getMemory v1
    m2 = Region.getMemory v2

-- | Perform transfer synchronously
performTransfer :: Transfer -> IO ()
performTransfer (Transfer link src dst) = case (link,src,dst) of

  -- (Host --> CL, Region1D, Region1D)
  (CLLink lib cq HostMemory (CLMemory {}), Region1D (Buffer _ (HostBuffer _ ptr)) soff sz, Region1D (Buffer _ (CLBuffer _ _ buf)) doff _) -> do
    let srcptr = plusPtr ptr (fromIntegral soff)
    e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
    void $ clReleaseEvent lib e

  -- (CL --> Host, Region1D, Region1D)
  (CLLink lib cq (CLMemory {}) HostMemory, Region1D (Buffer _ (CLBuffer _ _ buf)) soff sz, Region1D (Buffer _ (HostBuffer _ ptr)) doff _) -> do
      let dstptr = plusPtr ptr (fromIntegral doff)
      e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
      void $ clReleaseEvent lib e

  _ -> undefined


-- | Indicate if a data has a allocated (not valid) instance in a memory
isDataAllocatedR :: Data -> Memory -> R Bool
isDataAllocatedR d m = do
  allocs <- gets (invalidDataInstances ^$)
  let inst = Maybe.listToMaybe . filter ((== m) . getDataInstanceMemory) =<< Map.lookup d allocs
  return $ isJust inst

-- | Indicate if a data has a allocated (not valid) instance in any memory of the given list
isDataAllocatedAnyR :: Data -> [Memory] -> R Bool
isDataAllocatedAnyR d ms = do
  rs <- traverse (isDataAllocatedR d) ms
  return $ any id rs

-- | Get instances that can be detached, either because there are other
-- instances or because the data won't be used anymore by any other task
getDetachableInstancesR :: Data -> Memory -> R (Set DataInstance)
getDetachableInstancesR d mem = do
  
  allInstances <- gets (Map.findWithDefault [] d . getL datas)
  let (memInstances,otherInstances) = List.partition ((== mem) . getDataInstanceMemory) allInstances
  
  -- TODO: indicate that instances that won't be used by any other task are valid
  return $ if null otherInstances || invalidLength memInstances then Set.empty else Set.fromList memInstances

  where
    invalidLength [] = True
    invalidLength [_] = True
    invalidLength _ = False

-- | Get detachable instances for a data in a set of memories
getDetachableInstancesAnyR :: Data -> [Memory] -> R (Set DataInstance)
getDetachableInstancesAnyR d ms = fmap Set.unions $ traverse (getDetachableInstancesR d) ms

-- | Return invalid data instances
getInvalidDataInstancesR :: R (Map Data [DataInstance])
getInvalidDataInstancesR = gets (invalidDataInstances ^$)

-- | Return data instances (if any)
getInstancesR :: Data -> R [DataInstance]
getInstancesR d = gets (Map.findWithDefault [] d . getL datas)

-- | Return data events (if any)
getDataEventsR :: Data -> R [Event ()]
getDataEventsR d = gets (Map.findWithDefault [] d . getL dataEvents)

-- | Check for existing instance of a data
dataInstanceExistsR :: Data -> R Bool
dataInstanceExistsR d = do
  instances <- getInstancesR d
  return $ not (null instances)

-- | Return valid data instances
getDatasR :: R (Map Data [DataInstance])
getDatasR = gets (datas ^$)

-- | Return a new data identifier
newData :: DataDesc -> R Data
newData desc = do
  d <- gets (dataCounter ^$)
  modify (dataCounter ^%= (+) 1)
  lift $ return (Data d desc)

-- | Return a new buffer identifier
newBufferId :: R BufferId
newBufferId = do
  c <- gets (bufferCounter ^$)
  modify (bufferCounter ^%= (+) 1)
  lift $ return c

-- | Create a buffer
createBufferR :: Memory -> Word64 -> R Buffer
createBufferR mem sz = do
  buf <- allocBufferR mem sz
  registerBufferR mem buf
  lift $ return buf

-- | Release a buffer
releaseBuffer :: Buffer -> R ()
releaseBuffer buf = do
  let b = Buffer.getBufferImpl buf
  unregisterBufferR buf
  lift $ freeBuffer b

mapHostBufferR :: Word64 -> Ptr () -> R Buffer
mapHostBufferR sz ptr = do
  let bufImpl = HostBuffer sz ptr
  i <- newBufferId
  let buffer = Buffer i bufImpl
  registerBufferR HostMemory buffer
  return buffer

-- | Start a new asynchronous transfer
submitTransfer :: Transfer -> R ()
submitTransfer transfer@(Transfer link src dst) = do
  let valid = checkTransfer link src dst
  unless valid $ error "Invalid transfer"
  ch <- gets $ \x -> linkChannels x ! link
  lift $ writeChan ch transfer

-- | Allocate a buffer in the given memory
allocBufferR :: Memory -> Word64 -> R Buffer
allocBufferR m sz = do
  i <- newBufferId
  bufImpl <- lift $ allocBuffer m sz
  let buffer = Buffer i bufImpl
  modify (buffers ^%= List.insert buffer)
  return buffer

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = do
    (f,g) <- pMHelper p xs
    return (f [], g [])

pMHelper :: Monad m => (a -> m Bool) -> [a] -> m ([a] -> [a],[a] -> [a])
pMHelper p xs = foldM help (id,id) xs
      where
        help (f,g) x = do
            b <- p x
            return (if b then (f . (x:),g) else (f,g . (x:)))
