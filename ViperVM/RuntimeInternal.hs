{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections #-} 

module ViperVM.RuntimeInternal (
  -- Types
  Scheduler, Runtime(..), Message (..), TaskRequest(..),
  -- Methods
  startRuntime, voidR, putStrLnR,
  logCustomR, logInfoR, logWarningR, logErrorR,
  registerBuffer, registerDataInstance, newData,
  shutdownLogger,
  setEventR,
  getProcessorsR, getLoggerR, getChannelR, getDatasR,
  postMessageR, kpToTp, dataInstanceExistsR, getInstancesR,
  getCompiledKernelR,
  registerActiveRequestR, storeTaskRequestsR,
  isDataAllocatedR, isDataAllocatedAnyR,
  determineTaskRequests, allocBufferR, mapHostBufferR, filterRequestsR,
  storeCompiledKernelR,
  updateCompilationRequestsR, updateAllocationRequestsR, updateTransferRequestsR,
  -- Lenses
  submittedTasks, compiledKernels, scheduledTasks, dataTasks, invalidDataInstances, datas
  ) where

import Prelude hiding (lookup)

import Control.Applicative ( (<$>), liftA2 )
import Control.Concurrent
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Lens.Lazy
import Data.Lens.Template
import Data.List (intersect,partition)
import Data.Map (Map,alter,empty,lookup,fromList, (!))
import Data.Maybe (fromMaybe,isJust,isNothing,catMaybes)
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Word
import Foreign.Ptr
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import ViperVM.Buffer
import ViperVM.Data
import ViperVM.Event
import ViperVM.Kernel
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Task
import ViperVM.Transfer

-- | Messages that the runtime can handle.
data Message =  
    AppTaskSubmit KernelSet [Data] (Event [Data])-- ^ A task has been submitted by the application
  | AppQuit (Event ())            -- ^ Runtime shutdown is requested
  | AppMapVector DataDesc (Ptr ()) (Event Data) -- ^ A vector data is to be created using existing data
  | TaskSubmitted Task            -- ^ A task has been submitted
  | TaskScheduled Task Processor  -- ^ A task has been scheduled on a given processor
  | TaskComplete Task             -- ^ A task has completed
  | TaskReady Task                -- ^ A task has no associated request left
  | RequestsStored                -- ^ Some new requests have been submitted
  | KernelComplete Kernel         -- ^ A kernel has completed
  | KernelCompiled Kernel [Processor] [Maybe CompiledKernel] -- ^ A kernel compilation has completed
  | DataAllocated Data DataInstance -- ^ A placeholder has been allocated for the given data
  | DataTransfered Data DataInstance -- ^ A data has been transfered or duplicated

-- | Requests that are associated to tasks and that must be fulfilled before the task cen be executed
data TaskRequest = 
   RequestComputation Data                -- ^ Request that a data has been computed
 | RequestCompilation [Kernel] Processor  -- ^ Request compilation of at least one kernel for the given processor
 | RequestTransfer [Memory] Data          -- ^ Request the transfer of a data instance in any of the given memories
 | RequestDuplication [Memory] Data       -- ^ Request a duplicated instance (i.e. detachable) of the data in any of the given memories
 | RequestAllocation [Memory] Data        -- ^ Request the allocation of a placeholder for the given data in any of the given memories
 deriving (Eq,Ord,Show)

-- | State of the runtime system
data RuntimeState = RuntimeState {
  channel :: Chan Message,                  -- ^ Channel to communicate with the runtime
  platform :: Platform,                     -- ^ Platform used by the runtime
  logger :: Logger,                         -- ^ Logging method
  scheduler :: Scheduler,                   -- ^ Scheduler
  linkChannels :: Map Link (Chan Transfer), -- ^ Channels to communicate with link threads
  -- Lenses
  _buffers :: Map Memory [Buffer],          -- ^ Buffers in each memory
  _datas :: Map Data [DataInstance],        -- ^ Data and their valid instances
  _dataTasks :: Map Data Task,              -- ^ Task computing each (uncomputed) data
  _dataCounter :: Word,                     -- ^ Data counter (used to set data ID)
  _bufferCounter :: Word,                   -- ^ Buffer counter (used to set buffer ID)
  _invalidDataInstances :: Map Data [DataInstance], -- ^ Data and their invalid instances (just allocated, used in a transfer, etc.)
  _compiledKernels :: Map Kernel (Map Processor CompiledKernel), -- ^ Compiled kernel cache
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

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit (AppQuit _) = True
isQuit _ = False

-- | Starts the runtime on the given platform
startRuntime :: Platform -> Logger -> Scheduler -> IO Runtime
startRuntime pf l s = do
  ch <- newChan
  -- Create one chan per link
  lkChans <- fromList <$> liftA2 zip (return $ links pf) (replicateM (length $ links pf) newChan)
  let st = RuntimeState {
    channel = ch,
    platform = pf,
    logger = l,
    scheduler = s,
    linkChannels = lkChans,
    _buffers = empty,
    _datas = empty,
    _dataTasks = empty,
    _dataCounter = 0,
    _bufferCounter = 0,
    _invalidDataInstances = empty,
    _compiledKernels = empty,
    _submittedTasks = [],
    _scheduledTasks = fromList $ fmap (,[]) (processors pf),
    _requestTasks = empty,
    _taskRequests = empty,
    _activeRequests = Set.empty
  }
  void $ forkIO $ do
    logInfo l "Runtime started"
    msg <- evalStateT runtimeLoop st
    logInfo l "Stopping the runtime..."
    logInfo l "This log will now be closed"
    shutdownLogger l
    -- Signal that runtime is stopped to the application
    let (AppQuit ev) = msg
    setEvent ev ()
  return $ Runtime ch

-- | Main runtime loop
runtimeLoop :: R Message
runtimeLoop = do
  ch <- gets channel
  msg <- lift $ readChan ch

  sched <- gets scheduler
  sched msg

  if isQuit msg then return msg else runtimeLoop


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

-- | Register a request being fulfilled
registerActiveRequestR :: TaskRequest -> R ()
registerActiveRequestR req = modify(activeRequests ^%= Set.insert req)

-- | Store task requests
storeTaskRequestsR :: Task -> Set TaskRequest -> R ()
storeTaskRequestsR t reqs = do
  modify(taskRequests ^%= alter (const $ Just reqs) t)
  traverse_ (storeRequestTaskR t) reqs
  postMessageR RequestsStored
  
-- | Store a request if it doesn't already exist and associate a task to it
storeRequestTaskR :: Task -> TaskRequest -> R ()
storeRequestTaskR t r = do
  modify(requestTasks ^%= alter f r)
  where
    f (Just ts) = Just (t:ts)
    f Nothing = Just [t]

-- | Write a custom string message in the log
logCustomR :: String -> String -> R ()
logCustomR header s = do
  l <- gets logger
  lift $ logCustom l header s

-- | Write a custom error message in the log
logErrorR :: String -> R ()
logErrorR s = do
  l <- gets logger
  lift $ logError l s

-- | Write a custom warning message in the log
logWarningR :: String -> R ()
logWarningR s = do
  l <- gets logger
  lift $ logWarning l s

-- | Write a custom info message in the log
logInfoR :: String -> R ()
logInfoR s = do
  l <- gets logger
  lift $ logInfo l s

-- | Return processors of the platform
getProcessorsR :: R [Processor]
getProcessorsR = do
  pf <- gets platform
  return (processors pf)

-- | Return the logger
getLoggerR :: R Logger
getLoggerR = gets logger

-- | Return the message channel
getChannelR :: R (Chan Message)
getChannelR = gets channel

-- | Return the current datas
getDatasR :: R (Map Data [DataInstance])
getDatasR = gets (datas ^$)

-- | Post a message on the message channel
postMessageR :: Message -> R ()
postMessageR msg = do
  ch <- getChannelR
  lift $ writeChan ch msg

-- | Execute an action and return its start and end times
timeActionR :: IO a -> R (a,TimedAction)
timeActionR = lift . timeAction

-- | Shutdown the logger
shutdownLogger :: Logger -> IO ()
shutdownLogger l = void $ withNewEvent $ \e -> do
  l $ StopLogger e
  waitEvent e
  
-- | "Do nothing" in the R monad
voidR :: R ()
voidR = lift $ return ()

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

-- | Return data instances (if any)
getInstancesR :: Data -> R [DataInstance]
getInstancesR d = do
  ds <- getDatasR
  return $ Map.findWithDefault [] d ds

-- | Check for existing instance of a data
dataInstanceExistsR :: Data -> R Bool
dataInstanceExistsR d = do
  instances <- getInstancesR d
  return $ not (null instances)

-- | Indicate if a data has a allocated (not valid) instance in a memory
isDataAllocatedR :: Data -> Memory -> R Bool
isDataAllocatedR d m = do
  allocs <- gets (invalidDataInstances ^$)
  let inst = fmap (filter (\i -> m == getDataInstanceMemory i)) $ lookup d allocs
  return $ isJust inst

-- | Indicate if a data has a allocated (not valid) instance in any memory of the given list
isDataAllocatedAnyR :: Data -> [Memory] -> R Bool
isDataAllocatedAnyR d ms = do
  rs <- traverse (isDataAllocatedR d) ms
  return $ any id rs

-- | Return a compiled kernel from the cache, if any
getCompiledKernelR :: Processor -> Kernel -> R (Maybe CompiledKernel)
getCompiledKernelR p k = gets (getCompiledKernel p k . getL compiledKernels)

getCompiledKernel :: Processor -> Kernel -> Map Kernel (Map Processor CompiledKernel) -> Maybe CompiledKernel
getCompiledKernel p k cks = lookup k cks >>= lookup p

-- | Get instances that can be detached, either because there are other
-- instances or because the data won't be used anymore by any other task
getDetachableInstancesR :: Data -> Memory -> R (Set DataInstance)
getDetachableInstancesR d mem = do
  
  ds <- gets (datas ^$)
  let allInstances = fromMaybe [] $ lookup d ds
  let (memInstances,otherInstances) = partition (\i -> mem == getDataInstanceMemory i) allInstances
  
  -- TODO: indicate that instances that won't be used by any other task are valid
  return $ if null otherInstances || invalidLength memInstances then Set.empty else Set.fromList memInstances

  where
    invalidLength [] = True
    invalidLength [_] = True
    invalidLength _ = False

-- | Get detachable instances for a data in a set of memories
getDetachableInstancesAnyR :: Data -> [Memory] -> R (Set DataInstance)
getDetachableInstancesAnyR d ms = fmap Set.unions $ traverse (getDetachableInstancesR d) ms

-- | Determine requests that need to be fulfilled for a task to be scheduled on
-- a given processor
determineTaskRequests :: Processor -> Task -> R (Set TaskRequest)
determineTaskRequests proc task = do
  let Task (KernelSet ki ks) params = task

  -- Check that input data have been computed (i.e. have at least one instance)
  let inputs = roDatas params

  inputInstances <- traverse getInstancesR inputs
  let computeReqs = map RequestComputation $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstances inputs

  -- Check that there is an instance of each parameter available in memory
  let mems = attachedMemories proc
  let inputInstanceMemories = map (intersect mems . map getDataInstanceMemory) inputInstances
  let transferReqs = map (RequestTransfer mems) $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstanceMemories inputs

  -- Check that a kernel for the given proc has been compiled
  cced <- traverse (getCompiledKernelR proc) ks
  let compileReqs = if null (catMaybes cced) then [RequestCompilation ks proc] else []

  -- Check allocated output data
  let outputs = woDatas params
  isAllocatedOutput <- traverse (flip isDataAllocatedAnyR mems) outputs
  let allocReqs = map (RequestAllocation mems) $ catMaybes $ zipWith (\x y -> if x then Just y else Nothing) isAllocatedOutput outputs

  -- Checks for read-write data: we need to detect if an instance of the input
  -- data can be detached
  let rwInputs = rwInputDatas params
  detachableInstances <- traverse (flip getDetachableInstancesAnyR mems) rwInputs
  let duplicateReqs = map (RequestDuplication mems) $ catMaybes $ zipWith (\x y -> if Set.null x then Just y else Nothing) detachableInstances rwInputs

  let requests = Set.fromList $ computeReqs ++ transferReqs ++ compileReqs ++ allocReqs ++ duplicateReqs
  return requests

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

-- | Filter stored requests
filterRequestsR :: (TaskRequest -> Bool) -> R ()
filterRequestsR f = do
  modify(taskRequests ^%= Map.map (Set.filter f))
  modify(requestTasks ^%= Map.filterWithKey (\k _ -> f k))
  modify(activeRequests ^%= Set.filter f)

-- | Detect ready tasks
detectReadyTasksR :: R ()
detectReadyTasksR = do
  tasks <- gets(Map.keys . Map.filter Set.null . getL taskRequests)
  traverse_ (postMessageR . TaskReady) tasks

-- | Store a compiled kernel in state
storeCompiledKernelR :: Kernel -> CompiledKernel -> Processor -> R ()
storeCompiledKernelR k ck proc = modify $ compiledKernels ^%= Map.insertWith Map.union k (Map.singleton proc ck)

-- | Filter requests and detect ready tasks
updateRequestsR :: (TaskRequest -> Bool) -> R ()
updateRequestsR f = do
  filterRequestsR f
  detectReadyTasksR

-- | Remove compilation requests that have been fulfilled
updateCompilationRequestsR :: R ()
updateCompilationRequestsR = do
    cks <- gets(compiledKernels ^$)
    updateRequestsR (f cks)
  where
    f cks (RequestCompilation ks p) = all (\k -> isNothing $ getCompiledKernel p k cks) ks
    f _ _ = True
   
-- | Remove allocation requests that have been fulfilled
updateAllocationRequestsR :: R ()
updateAllocationRequestsR = do
    instances <- gets(invalidDataInstances ^$)
    updateRequestsR (f instances)
  where
    f instances (RequestAllocation mems d) = null $ intersect mems $ map getDataInstanceMemory $ Map.findWithDefault [] d instances
    f _ _ = True

-- | Remove transfer requests that have been fulfilled
updateTransferRequestsR :: R ()
updateTransferRequestsR = do
    ds <- gets(datas ^$)
    updateRequestsR (f ds)
  where
    f ds (RequestTransfer mems d) = null $ intersect mems $ map getDataInstanceMemory $ Map.findWithDefault [] d ds
    f _ _ = True
