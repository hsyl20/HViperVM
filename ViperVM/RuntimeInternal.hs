{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections #-} 

module ViperVM.RuntimeInternal (
  -- Types
  Scheduler, Runtime(..), Message (..),
  -- Methods
  startRuntime, voidR,
  logCustomR, logInfoR, logWarningR, logErrorR,
  registerBuffer, registerDataInstance, newData,
  shutdownLogger,
  setEventR,
  getProcessorsR, getLoggerR, getChannelR, postMessageR, paramToData,
  -- Lenses
  submittedTasks, compiledKernels, scheduledTasks
  ) where

import Control.Applicative ( (<$>), liftA2 )
import Control.Concurrent
import Control.Monad.State
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Map
import Data.Word
import Foreign.Ptr
import qualified Data.List as List

import ViperVM.Buffer
import ViperVM.Data
import ViperVM.Event
import ViperVM.Kernel
import ViperVM.KernelInterface
import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Task
import ViperVM.Transfer

-- | Messages that the runtime can handle.
data Message =  SubmitTask Task (Event [Data])-- ^ A task has been submitted by the application
              | TaskScheduled Task Processor  -- ^ A task has been scheduled on a given processor
              | TaskComplete Task             -- ^ A task has completed
              | KernelComplete Kernel         -- ^ A kernel has completed
              | KernelCompiled Kernel [Processor] [Maybe CompiledKernel] -- ^ A kernel compilation has completed
              | TransferComplete Transfer     -- ^ A data transfer has completed
              | MapVector DataDesc (Ptr ()) (Event Data) -- ^ A vector data is to be created using existing data
              | DataRelease Data              -- ^ A data is no longer necessary
              | Quit (Event ())               -- ^ Runtime shutdown is requested

-- | State of the runtime system
data RuntimeState = RuntimeState {
  channel :: Chan Message,                  -- ^ Channel to communicate with the runtime
  platform :: Platform,                     -- ^ Platform used by the runtime
  logger :: Logger,                         -- ^ Logging method
  scheduler :: Scheduler,                   -- ^ Scheduler
  linkChannels :: Map Link (Chan Transfer), -- ^ Channels to communicate with link threads
  -- Lenses
  _buffers :: Map Memory [Buffer],          -- ^ Buffers in each memory
  _activeTransfers :: [Transfer],           -- ^ Current data transfers
  _datas :: Map Data [DataInstance],        -- ^ Registered data
  _dataCounter :: Word,                     -- ^ Data counter (used to set data ID)
  _compiledKernels :: Map Kernel (Map Processor CompiledKernel), -- ^ Compiled kernel cache
  _submittedTasks :: [(Task,[Data])],       -- ^ Tasks that are to be scheduled
  _scheduledTasks :: Map Processor [Task]   -- ^ Tasks scheduled on processors (may be executing)
}

type R = StateT RuntimeState IO
type Scheduler = Message -> R ()

newtype Runtime = Runtime (Chan Message)

$( makeLens ''RuntimeState )

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit (Quit _) = True
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
    _activeTransfers = [],
    _datas = empty,
    _dataCounter = 0,
    _compiledKernels = empty,
    _submittedTasks = [],
    _scheduledTasks = fromList $ fmap (,[]) (processors pf)
  }
  void $ forkIO $ do
    logInfo l "Runtime started"
    msg <- evalStateT runtimeLoop st
    logInfo l "Stopping the runtime..."
    logInfo l "This log will now be closed"
    shutdownLogger l
    -- Signal that runtime is stopped to the application
    let (Quit ev) = msg
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
  registerActiveTransfer transfer
  ch <- gets $ \x -> linkChannels x ! link
  lift $ writeChan ch transfer

-- | Register an active transfer
registerActiveTransfer :: Transfer -> R ()
registerActiveTransfer transfer = modify(activeTransfers ^%= (++) [transfer])

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

-- | Schedule the execution of a given compiled kernel on the specified processor
-- Kernel parameters (i.e. data instances) must be stored in appropriate memories
runCompiledKernelOn :: Processor -> CompiledKernel -> [DataInstance] -> R (Event [DataInstance])
runCompiledKernelOn proc k params = undefined
  
-- Set an event in the R monad
setEventR :: Event a -> a -> R ()
setEventR ev v = lift $ setEvent ev v


-- | Create data handle for a kernel parameter
paramToData :: Parameter -> R Data
paramToData (ReadOnly d) = return d
paramToData (ReadWrite d) = do
  d2 <- newData (dataDescriptor d)
  return d2
paramToData (Allocate dd) = do
  d2 <- newData dd
  return d2
