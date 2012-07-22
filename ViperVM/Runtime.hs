{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Runtime (
  -- Types
  R, Scheduler, Runtime(..), Message (..),
  -- Methods
  startRuntime, stopRuntime,
  logInfo, logWarning,
  registerBuffer, registerDataInstance, newData,
  ) where

import Control.Applicative ( (<$>), liftA2 )
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State
import Data.Lens.Lazy
import Data.Lens.Template
import qualified Data.List as List
import Data.Map
import Data.Word
import Foreign.Ptr

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Transfer
import ViperVM.Task
import ViperVM.Data
import ViperVM.Kernel
import ViperVM.Logging.Logger

-- | Messages that the runtime can handle.
-- Do not use them directly as helpers functions are provided
data Message = TaskSubmit Task | 
               KernelComplete Kernel | 
               TransferComplete Transfer |
               MapVector VectorDesc (Ptr ()) (MVar Data) |
               DataRelease Data |
               Quit


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
  _dataCounter :: Word                      -- ^ Data counter (used to set data ID)
}

type R = StateT RuntimeState IO
type Scheduler = Message -> R ()

newtype Runtime = Runtime (Chan Message)

$( makeLens ''RuntimeState )

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit Quit = True
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
    _dataCounter = 0
  }
  void $ forkIO $ do
    void $ execStateT runtime st
    return ()
  return $ Runtime ch

-- | Main runtime loop
runtime :: R ()
runtime = do
  ch <- gets channel
  msg <- lift $ readChan ch

  unless (isQuit msg) $ do
    s <- gets scheduler
    s msg
    runtime


-- | Stop the given runtime
stopRuntime :: Runtime -> IO ()
stopRuntime (Runtime ch) = writeChan ch Quit 

-- | Return a new data identifier
newData :: R Data
newData = do
  d <- gets (dataCounter ^$)
  modify (dataCounter ^%= (+) 1)
  lift $ return (Data d)

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
  ch <- gets $ \x -> (linkChannels x) ! link
  lift $ writeChan ch transfer

-- | Register an active transfer
registerActiveTransfer :: Transfer -> R ()
registerActiveTransfer transfer = modify(activeTransfers ^%= (++) [transfer])

logWarning :: String -> R ()
logWarning = logCustom "Warning" 

logInfo :: String -> R ()
logInfo = logCustom "Info" 

logCustom :: String -> String -> R ()
logCustom header s = do
  l <- gets logger
  lift $ l $ Custom $ header ++ ": " ++ s
