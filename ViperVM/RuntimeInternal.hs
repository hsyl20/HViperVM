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
  registerActiveRequestR, registerTaskRequestsR,
  -- Lenses
  submittedTasks, compiledKernels, scheduledTasks, dataTasks
  ) where

import Prelude hiding (lookup)

import Control.Applicative ( (<$>), liftA2 )
import Control.Concurrent
import Control.Monad.State
import Data.Foldable
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Foreign.Ptr
import qualified Data.List as List

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
data Message =  AppTaskSubmit KernelSet [Data] (Event [Data])-- ^ A task has been submitted by the application
              | AppQuit (Event ())            -- ^ Runtime shutdown is requested
              | AppMapVector DataDesc (Ptr ()) (Event Data) -- ^ A vector data is to be created using existing data
              | TaskSubmitted Task            -- ^ A task has been submitted
              | TaskScheduled Task Processor  -- ^ A task has been scheduled on a given processor
              | TaskComplete Task             -- ^ A task has completed
              | Request TaskRequest           -- ^ A new task request has been submitted
              | KernelComplete Kernel         -- ^ A kernel has completed
              | KernelCompiled Kernel [Processor] [Maybe CompiledKernel] -- ^ A kernel compilation has completed
              | TransferComplete Transfer     -- ^ A data transfer has completed
              | DataRelease Data              -- ^ A data is no longer necessary

data TaskRequest = RequestComputation Data
                 | RequestCompilation [Kernel] Processor
                 | RequestTransfer [Memory] Data
                 | RequestDuplication Data Data Memory
                 | RequestAllocation Data Memory
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
  _datas :: Map Data [DataInstance],        -- ^ Registered data
  _dataTasks :: Map Data Task,              -- ^ Task computing each (uncomputed) data
  _dataCounter :: Word,                     -- ^ Data counter (used to set data ID)
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
  buf <- lift $ allocBuffer mem sz
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

-- | Register task requests
registerTaskRequestsR :: Task -> Set TaskRequest -> R ()
registerTaskRequestsR t reqs = do
  modify(taskRequests ^%= alter (const $ Just reqs) t)
  traverse_ (registerRequestTaskR t) reqs
  
-- | Register a request if it doesn't already exist and associate a task to it
registerRequestTaskR :: Task -> TaskRequest -> R ()
registerRequestTaskR t r = do
  modify(requestTasks ^%= alter f r)
  postMessageR $ Request r
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
  return $ fromMaybe [] $ lookup d ds

-- | Check for existing instance of a data
dataInstanceExistsR :: Data -> R Bool
dataInstanceExistsR d = do
  ds <- getDatasR
  let n = fromMaybe 0 $ fmap length $ lookup d ds
  return $ n /= 0

-- | Return a compiled kernel from the cache, if any
getCompiledKernelR :: Processor -> Kernel -> R (Maybe CompiledKernel)
getCompiledKernelR p k = do
  cced <- gets (compiledKernels ^$)
  return $ lookup p =<< lookup k cced
