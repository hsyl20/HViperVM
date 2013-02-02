{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Structures where

import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Event
import ViperVM.Buffer
import ViperVM.BufferManager
import ViperVM.Data
import ViperVM.DataManager
import ViperVM.Event
import ViperVM.InstanceManager
import ViperVM.Kernel
import ViperVM.KernelManager
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Region
import ViperVM.RegionManager
import ViperVM.Task
import ViperVM.Transfer

import qualified ViperVM.BufferManager as BufferManager
import qualified ViperVM.DataManager as DataManager
import qualified ViperVM.InstanceManager as InstanceManager
import qualified ViperVM.KernelManager as KernelManager

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Arrow ((***))

import Data.Lens.Lazy
import Data.Lens.Template
import Data.Map (Map,(!))
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Word
import Foreign.Ptr

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Messages that the runtime can handle.
data Message =  
    AppTaskSubmit KernelSet [Data] (Event [Data])-- ^ A task has been submitted by the application
  | AppQuit (Event ())            -- ^ Runtime shutdown is requested
  | AppMapVector DataDesc (Ptr ()) (Event Data) -- ^ A vector data is to be created using existing data
  | AppWaitForData Data (Event ()) -- ^ Synchronously wait for data computation
  | TaskSubmitted Task            -- ^ A task has been submitted
  | TaskScheduled Task Processor  -- ^ A task has been scheduled on a given processor
  | TaskComplete Task             -- ^ A task has completed
  | TaskReady Task                -- ^ A task has no associated request left on proc
  | RequestsStored                -- ^ Some new requests have been submitted
  | KernelComplete Kernel         -- ^ A kernel has completed
  | KernelCompiled Kernel [Processor] [Maybe CompiledKernel] -- ^ A kernel compilation has completed
  | DataAllocated Data DataInstance -- ^ A placeholder has been allocated for the given data
  | DataTransfered Data DataInstance -- ^ A data has been transfered or duplicated
  | DataComputed Data             -- ^ A data has been computed

-- | Requests that are associated to tasks and that must be fulfilled before the task cen be executed
data TaskRequest = 
   RequestComputation Data              -- ^ Request that a data has been computed
 | RequestCompilation [Kernel] Processor  -- ^ Request compilation of at least one kernel for the given processor
 | RequestTransfer [Memory] Data        -- ^ Request the transfer of a data instance in any of the given memories
 | RequestDuplication [Memory] Data     -- ^ Request a duplicated instance (i.e. detachable) of the data in any of the given memories
 | RequestAllocation [Memory] Data      -- ^ Request the allocation of a placeholder for the given data in any of the given memories
 deriving (Eq,Ord)

instance Show TaskRequest where
  show (RequestComputation d) = "Computation of " ++ show d 
  show (RequestCompilation ks _) = "Compilation of any of " ++ show ks
  show (RequestTransfer ms d) = "Transfer of " ++ show d ++ " in any of " ++ show ms
  show (RequestDuplication ms d) = "Duplication of " ++  show d ++ " in any of " ++ show ms
  show (RequestAllocation ms d) = "Allocation of " ++  show d ++ " in any of " ++ show ms

-- | State of the runtime system
data RuntimeState = RuntimeState {
  channel :: Chan Message,                  -- ^ Channel to communicate with the runtime
  platform :: Platform,                     -- ^ Platform used by the runtime
  logger :: Logger,                         -- ^ Logging method
  scheduler :: Scheduler,                   -- ^ Scheduler
  linkChannels :: Map Link (Chan Transfer), -- ^ Channels to communicate with link threads
  -- Lenses
  _bufferManager :: BufferManager,          -- ^ Buffer manager
  _regionManager :: RegionManager,          -- ^ Region manager
  _instanceManager :: InstanceManager,      -- ^ Data instance manager
  _dataManager   :: DataManager,            -- ^ Data manager
  _kernelManager   :: KernelManager,        -- ^ Kernel manager

  _dataEvents :: Map Data [Event ()],       -- ^ Data and waiting events
  _dataTasks :: Map Data Task,              -- ^ Task computing each (uncomputed) data
  _invalidDataInstances :: Map Data [DataInstance], -- ^ Data and their invalid instances (just allocated, used in a transfer, etc.)
  _submittedTasks :: [Task],                -- ^ Tasks that are to be scheduled
  _scheduledTasks :: Map Processor [Task],  -- ^ Tasks scheduled on processors (may be executing)
  _requestTasks :: Map TaskRequest [Task],  -- ^ Requests and tasks that have made the request
  _taskRequests :: Map Task (Set TaskRequest),  -- ^ Tasks and the request the have made
  _activeRequests :: Set TaskRequest
}

type R = StateT RuntimeState IO
type Scheduler = Message -> R ()

newtype Runtime = Runtime (Chan Message)

$( makeLens ''RuntimeState )


-- | Execute an IO action using the runtime state
withStateR :: (RuntimeState -> IO a) -> R a
withStateR f = do
  st <- get
  lift $ f st

-- | Execute an IO action using the runtime state and drop the result
withStateR_ :: (RuntimeState -> IO a) -> R ()
withStateR_ f = do
  st <- get
  lift $ void $ f st

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit (AppQuit _) = True
isQuit _ = False

-- | Post a message on the message channel
postMessageR :: Message -> R ()
postMessageR msg = withStateR_ $ flip writeChan msg . channel

-- | "Do nothing" in the R monad
voidR :: R ()
voidR = lift $ return ()

----------------------------------------------------------
-- Accessors
----------------------------------------------------------

-- | Return processors of the platform
getProcessorsR :: R [Processor]
getProcessorsR = gets (processors . platform)

-- | Return the logger
getLoggerR :: R Logger
getLoggerR = gets logger

-- | Return the message channel
getChannelR :: R (Chan Message)
getChannelR = gets channel

-- | Return scheduled tasks
getScheduledTasksR :: R (Map Processor [Task])
getScheduledTasksR = access scheduledTasks

getKernelManagerR :: R KernelManager
getKernelManagerR = access kernelManager

-- | Return a compiled kernel from the cache, if any
getCompiledKernelR :: Processor -> Kernel -> R (Maybe CompiledKernel)
getCompiledKernelR p k = gets (\s -> compiledFor (kernelManager ^$ s) p k)

-- | Associate a compiled kernel
storeCompiledKernelR :: Kernel -> CompiledKernel -> Processor -> R ()
storeCompiledKernelR k ck proc = modify ( kernelManager ^%= \mgr -> KernelManager.associate mgr k proc ck)

isLinking :: Memory -> Memory -> Link -> Bool
isLinking m1 m2 l = ms == (m1,m2) || ms == (m2,m1)
  where
    ms = linkEndpoints l 

-- | Get links between memories
getLinksBetweenMemoriesR :: Memory -> Memory -> R [Link]
getLinksBetweenMemoriesR m1 m2 = gets (filter (isLinking m1 m2) . links . platform)

-- | Memory containing the given buffer
getBufferMemoryR :: Buffer -> R Memory
getBufferMemoryR buffer = flip bufferMemory buffer <$> gets (bufferManager ^$)

-- Get links between two data instances
getLinksBetweenDataInstances :: DataInstance -> DataInstance -> R [Link]
getLinksBetweenDataInstances d1 d2 = do
   m1 <- getBufferMemoryR (getDataInstanceBuffer d1)
   m2 <- getBufferMemoryR (getDataInstanceBuffer d2)
   getLinksBetweenMemoriesR m1 m2

-- | Register a data with an initial instance
associateDataInstanceR :: Data -> DataInstance -> R ()
associateDataInstanceR d di = modify (instanceManager ^%= \mgr -> InstanceManager.associate mgr d di)

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

-- | Retrieve data list
datasR :: R [Data]
datasR = gets (\s -> datas (dataManager ^$ s))

-- | Register data event
registerDataEventR :: Data -> Event () -> R ()
registerDataEventR d ev = modify (dataEvents ^%= modDataEvents)
  where
    modDataEvents = Map.alter f d
    f (Just x) = Just (x ++ [ev])
    f Nothing  = Just [ev]

-- | Map host buffer
mapHostBufferR :: Word64 -> Ptr () -> R Buffer
mapHostBufferR sz ptr = do
   mgr <- getBufferManagerR
   let (mgr2,buffer) = mapHostBuffer mgr sz ptr
   putBufferManagerR mgr2
   return buffer

-- | Return memory containing a data instance
getDataInstanceMemoryR :: DataInstance -> R Memory
getDataInstanceMemoryR (Vector b _) = getBufferMemoryR b

checkTransfer :: Transfer -> R Bool
checkTransfer (Transfer l b1 r1 b2 r2) = do
  m1 <- getBufferMemoryR b1
  m2 <- getBufferMemoryR b2
  return $ lm1 == m1 && lm2 == m2 && checkCompatibleRegions r1 r2
  where
    (lm1,lm2) = linkEndpoints l

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
  return $ elem m mems

-- | Indicate if a data has a allocated (not valid) instance in any memory of the given list
isDataAllocatedAnyR :: Data -> [Memory] -> R Bool
isDataAllocatedAnyR d ms = or <$> traverse (isDataAllocatedR d) ms

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
partitionM p xs = (map fst *** map fst) . List.partition snd . zip xs <$> traverse p xs

-- | Execute an action and return its start and end times
timeActionR :: IO a -> R (a,TimedAction)
timeActionR = lift . timeAction

-- Set an event in the R monad
setEventR :: Event a -> a -> R ()
setEventR ev v = lift $ setEvent ev v

-- | Convert kernel parameter into task parameter, allocating data when necessary
kpToTp :: KernelParameter -> R TaskParameter
kpToTp (KPReadOnly d) = return (TPReadOnly d)
kpToTp (KPReadWrite d) = TPReadWrite d <$> (allocateDataR =<< descriptorR d)
kpToTp (KPAllocate dd) = TPAllocate <$> allocateDataR dd
