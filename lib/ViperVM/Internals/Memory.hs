module ViperVM.Internals.Memory where

import ViperVM.Buffer
import ViperVM.Data
import ViperVM.Event
import ViperVM.Internals.Structures
import ViperVM.Platform
import ViperVM.Transfer
import ViperVM.Region
import Foreign.Ptr
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Event
import Data.Traversable (traverse)
import Data.Lens.Lazy
import Data.Word
import Data.Map (Map,(!))
import Data.Set (Set)
import Control.Monad.State
import Control.Applicative
import Control.Concurrent.Chan
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified ViperVM.Internals.InstanceManager as InstanceManager
import qualified ViperVM.Internals.BufferManager as BufferManager
import qualified ViperVM.Internals.DataManager as DataManager
import ViperVM.Internals.BufferManager 
import ViperVM.Internals.InstanceManager
import ViperVM.Internals.DataManager

-- | Register a data with an initial instance
registerDataInstanceR :: Data -> DataInstance -> R ()
registerDataInstanceR d di = modify (instanceManager ^%= \mgr -> InstanceManager.associate mgr d di)

-- | Retrieve buffer manager
getBufferManagerR :: R BufferManager
getBufferManagerR = gets (bufferManager ^$)

-- | Store buffer manager
putBufferManagerR :: BufferManager -> R ()
putBufferManagerR mgr = modify (bufferManager ^= mgr)

-- | Retrieve instance manager
getInstanceManagerR :: R InstanceManager
getInstanceManagerR = gets (instanceManager ^$)

-- | Store instance manager
putInstanceManagerR :: InstanceManager -> R ()
putInstanceManagerR mgr = modify (instanceManager ^= mgr)

-- | Retrieve data manager
getDataManagerR :: R DataManager
getDataManagerR = gets (dataManager ^$)

-- | Store data manager
putDataManagerR :: DataManager -> R ()
putDataManagerR mgr = modify (dataManager ^= mgr)

-- | Allocate a new buffer
allocateBufferR :: Memory -> Word64 -> R (Maybe Buffer)
allocateBufferR mem sz = do
   mgr <- getBufferManagerR
   (mgr2,buf) <- lift $ BufferManager.allocate mgr mem sz
   putBufferManagerR mgr2
   return buf

-- | Free a buffer
freeBufferR :: Buffer -> R ()
freeBufferR buf = getBufferManagerR >>= lift . flip BufferManager.free buf >>= putBufferManagerR

-- | Allocate a new data
allocateDataR :: DataDesc -> R Data
allocateDataR desc = do
   mgr <- getDataManagerR
   let (mgr2,d) = DataManager.allocate mgr desc
   putDataManagerR mgr2
   return d

-- | Release a data
releaseDataR :: Data -> R ()
releaseDataR d = modify (dataManager ^%= flip DataManager.release d)

-- | Retrieve instances of a data
instancesR :: Data -> R [DataInstance]
instancesR d = gets (\s -> InstanceManager.instances (instanceManager ^$ s) d)

-- | Retrieve data descriptor
descriptorR :: Data -> R DataDesc
descriptorR d = gets(\s -> descriptor (dataManager ^$ s) d)

-- | Register data event
registerDataEventR :: Data -> Event () -> R ()
registerDataEventR d ev = modify (dataEvents ^%= modDataEvents)
  where
    modDataEvents = Map.alter f d
    f (Just x) = Just (x ++ [ev])
    f Nothing  = Just [ev]

-- | Return memory containing a data instance
getDataInstanceMemoryR :: DataInstance -> R Memory
getDataInstanceMemoryR (Vector b _) = getBufferMemoryR b

checkTransfer :: Transfer -> R Bool
checkTransfer (Transfer l b1 r1 b2 r2) = do
  m1 <- getBufferMemoryR b1
  m2 <- getBufferMemoryR b2
  return $ lm1 == m1 && lm2 == m2 && checkCompatibleRegions r1 r2
  where
    (lm1,lm2) = getLinkMemories l

-- | Perform transfer synchronously
performTransfer :: Transfer -> IO ()
performTransfer (Transfer link srcBuf srcReg dstBuf dstReg) = case (link,srcBuf,srcReg,dstBuf,dstReg) of

  -- Host --> CL, Region1D, Region1D
  (CLLink lib cq HostMemory (CLMemory {}), 
      HostBuffer _ ptr, Region1D soff sz,
      CLBuffer _ _ buf, Region1D doff _) -> do
         let srcptr = plusPtr ptr (fromIntegral soff)
         e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
         void $ clWaitForEvents lib [e]
         void $ clReleaseEvent lib e

  -- CL --> Host, Region1D, Region1D
  (CLLink lib cq (CLMemory {}) HostMemory,
      CLBuffer _ _ buf, Region1D soff sz,
      HostBuffer _ ptr, Region1D doff _) -> do
         let dstptr = plusPtr ptr (fromIntegral doff)
         e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
         void $ clWaitForEvents lib [e]
         void $ clReleaseEvent lib e

  _ -> undefined

-- | Indicate if a data instance if stored in the given memory
isInstanceInMem :: Memory -> DataInstance -> R Bool
isInstanceInMem mem di = (== mem) <$> getDataInstanceMemoryR di

-- | Indicate if a data has a allocated (not valid) instance in a memory
isDataAllocatedR :: Data -> Memory -> R Bool
isDataAllocatedR d m = do
  insts <- Map.findWithDefault [] d <$> gets (invalidDataInstances ^$)
  mems <- traverse getDataInstanceMemoryR insts
  return $ any (== m) mems

-- | Indicate if a data has a allocated (not valid) instance in any memory of the given list
isDataAllocatedAnyR :: Data -> [Memory] -> R Bool
isDataAllocatedAnyR d ms = do
  rs <- traverse (isDataAllocatedR d) ms
  return $ any id rs

-- | Get instances that can be detached, either because there are other
-- instances or because the data won't be used anymore by any other task
getDetachableInstancesR :: Data -> Memory -> R (Set DataInstance)
getDetachableInstancesR d mem = do
  
  (memInstances,otherInstances) <- partitionM (isInstanceInMem mem) =<< instancesR d
  
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

-- | Return data events (if any)
getDataEventsR :: Data -> R [Event ()]
getDataEventsR d = gets (Map.findWithDefault [] d . getL dataEvents)

-- | Check for existing instance of a data
dataInstanceExistsR :: Data -> R Bool
dataInstanceExistsR d = not . null <$> instancesR d

-- | Start a new asynchronous transfer
submitTransfer :: Transfer -> R ()
submitTransfer transfer@(Transfer link _ _ _ _) = do
  valid <- checkTransfer transfer
  unless valid $ error "Invalid transfer"
  ch <- gets $ \x -> linkChannels x ! link
  lift $ writeChan ch transfer

partitionM :: Applicative m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = ( \ (a,b) -> (map fst a, map fst b)) . List.partition snd . zip xs <$> traverse p xs
