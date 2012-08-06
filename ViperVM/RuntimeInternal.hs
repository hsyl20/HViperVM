module ViperVM.RuntimeInternal (
  -- Methods
  putStrLnR,
  registerBuffer, registerDataInstance, newData,
  setEventR,
  getProcessorsR, getLoggerR, getChannelR, getDatasR,
  postMessageR, kpToTp, dataInstanceExistsR, getInstancesR,
  getCompiledKernelR,
  allocBufferR, mapHostBufferR,
  storeCompiledKernelR,
  ) where

import Prelude hiding (lookup)

import Control.Concurrent
import Control.Monad.State
import Data.Lens.Lazy
import Data.Map (Map,alter, (!))
import Data.Word
import Foreign.Ptr
import qualified Data.Map as Map
import qualified Data.List as List

import ViperVM.Internals.Structures

import ViperVM.Buffer
import ViperVM.Data
import ViperVM.Event
import ViperVM.Kernel
import ViperVM.KernelInterface
import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Task
import ViperVM.Transfer


-- | Return a new data identifier
newData :: DataDesc -> R Data
newData desc = do
  d <- gets (dataCounter ^$)
  modify (dataCounter ^%= (+) 1)
  lift $ return (Data d desc)

-- | Return a new buffer identifier
newBufferID :: R BufferID
newBufferID = do
  c <- gets (bufferCounter ^$)
  modify (bufferCounter ^%= (+) 1)
  lift $ return c

-- | Register a data with an initial instance
registerDataInstance :: Data -> DataInstance -> R ()
registerDataInstance d di = modify (datas ^%= modDatas)
  where
    modDatas = alter f d
    f (Just x) = Just (x ++ [di])
    f Nothing  = Just [di]

-- | Create a buffer
createBuffer :: Memory -> Word64 -> R Buffer
createBuffer mem sz = do
  buf <- allocBufferR mem sz
  registerBuffer mem buf
  lift $ return buf

-- | Release a buffer
releaseBuffer :: Buffer -> R ()
releaseBuffer buf = do
  unregisterBuffer buf
  lift $ freeBuffer buf

-- | Register a new buffer
registerBuffer :: Memory -> Buffer -> R ()
registerBuffer mem buf = modify (buffers ^%= modBuffer)
  where
    modBuffer = alter f mem
    f (Just x) = Just (x ++ [buf])
    f Nothing  = Just [buf]

-- | Unregister a buffer
unregisterBuffer :: Buffer -> R ()
unregisterBuffer buf = modify (buffers ^%= modBuffer)
  where
    mem = getBufferMemory buf
    modBuffer :: Map Memory [Buffer] -> Map Memory [Buffer]
    modBuffer = alter (fmap $ List.delete buf) mem

-- | Start a new asynchronous transfer
submitTransfer :: Transfer -> R ()
submitTransfer transfer@(Transfer link _ _) = do
  lift $ unless (checkTransfer transfer) $ error "Invalid transfer"
  ch <- gets $ \x -> linkChannels x ! link
  lift $ writeChan ch transfer

-- | Execute an action and return its start and end times
timeActionR :: IO a -> R (a,TimedAction)
timeActionR = lift . timeAction

-- | putStrLn in the R monad
putStrLnR :: String -> R ()
putStrLnR s = lift $ putStrLn s

-- Set an event in the R monad
setEventR :: Event a -> a -> R ()
setEventR ev v = lift $ setEvent ev v

-- | Convert kernel parameter into task parameter, allocating data when necessary
kpToTp :: KernelParameter -> R TaskParameter
kpToTp (KPReadOnly d) = return $ (TPReadOnly d)
kpToTp (KPReadWrite d) = do
  d2 <- newData (dataDescriptor d)
  return $ TPReadWrite d d2
kpToTp (KPAllocate dd) = do
  d2 <- newData dd
  return $ TPAllocate d2

-- | Allocate a buffer in the given memory
allocBufferR :: Memory -> Word64 -> R Buffer
allocBufferR m sz = do
  i <- newBufferID
  buf <- lift $ allocBuffer i m sz
  return buf

-- | Map a host buffer
mapHostBufferR :: Word64 -> Ptr () -> R Buffer
mapHostBufferR sz ptr = do
  i <- newBufferID
  return $ HostBuffer i sz ptr

-- | Store a compiled kernel in state
storeCompiledKernelR :: Kernel -> CompiledKernel -> Processor -> R ()
storeCompiledKernelR k ck proc = modify $ compiledKernels ^%= Map.insertWith Map.union k (Map.singleton proc ck)
