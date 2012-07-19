{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Scheduler (
  startScheduler,
  stopScheduler
  ) where

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Transfer
import ViperVM.Task
import ViperVM.Data
import ViperVM.View

import Control.Concurrent
import Control.Applicative ( (<$>), liftA2 )
import Control.Monad.State
import Data.Map
import Data.Word
import qualified Data.List as List
import Data.Lens.Lazy
import Data.Lens.Template
import Foreign.Ptr

data Message = TaskSubmit Task | 
               KernelComplete Kernel | 
               TransferComplete Transfer |
               MapVector VectorDesc (Ptr ()) (MVar Data) |
               DataRelease Data |
               Quit

data SchedState = SchedState {
  _channel :: Chan Message,
  _platform :: Platform,
  _buffers :: Map Memory [Buffer],
  _activeTransfers :: [Transfer],
  _linkChans :: Map Link (Chan Transfer),
  _datas :: Map Data [DataInstance],
  _dataCounter :: Word
}

$( makeLens ''SchedState ) 

data Scheduler = Scheduler (Chan Message)

-- | Starts the scheduler on the given platform
startScheduler :: Platform -> IO Scheduler
startScheduler pf = do
  (st,ch) <- initSchedState pf
  _ <- forkIO $ do
    _ <- execStateT scheduler st
    return ()
  return $ Scheduler ch

-- | Stop the given scheduler
stopScheduler :: Scheduler -> IO ()
stopScheduler (Scheduler ch) = writeChan ch Quit 

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit Quit = True
isQuit _ = False

-- | Initialize scheduler state
initSchedState :: Platform -> IO (SchedState,Chan Message)
initSchedState pf = do
  ch <- newChan
  lkChans <- fromList <$> liftA2 zip (return $ links pf) (replicateM (length $ links pf) newChan)
  st <- return SchedState {
    _channel = ch,
    _platform = pf,
    _buffers = empty,
    _activeTransfers = [],
    _linkChans = lkChans,
    _datas = empty,
    _dataCounter = 0
  }
  return (st,ch)

-- | Launch the scheduler on the given platform. Communication is done through the channel
scheduler :: StateT SchedState IO ()
scheduler = do
  ch <- gets (channel ^$)
  msg <- lift $ readChan ch
  case msg of
    TaskSubmit t -> taskSubmit t
    KernelComplete k -> kernelComplete k
    TransferComplete t -> transferComplete t
    MapVector desc ptr r -> mapVector desc ptr r
    DataRelease d -> dataRelease d
    Quit -> lift $ return ()
  unless (isQuit msg) scheduler

  where

  taskSubmit :: Task -> StateT SchedState IO ()
  taskSubmit t = undefined

  kernelComplete :: Kernel -> StateT SchedState IO ()
  kernelComplete k = undefined

  transferComplete :: Transfer -> StateT SchedState IO ()
  transferComplete t = undefined

  mapVector :: VectorDesc -> Ptr () -> MVar Data -> StateT SchedState IO ()
  mapVector desc@(VectorDesc prim n) ptr r = do
    let sz = n * (primitiveSize prim)
    buf <- lift . return $ HostBuffer sz ptr
    registerBuffer HostMemory buf
    let view = View1D buf 0 sz
    let di = Vector desc view
    d <- newData
    registerDataInstance d di
    lift $ putMVar r d

  dataRelease :: Data -> StateT SchedState IO ()
  dataRelease d = undefined

-- | Return a new data identifier
newData :: StateT SchedState IO Data
newData = do
  d <- gets (dataCounter ^$)
  modify (dataCounter ^%= (+) 1)
  lift $ return (Data d)

-- | Register a data with an initial instance
registerDataInstance :: Data -> DataInstance -> StateT SchedState IO ()
registerDataInstance d di = modify (datas ^%= modDatas)
  where
    modDatas = alter f d
    f (Just x) = Just (x ++ [di])
    f Nothing  = Just [di]

-- | Create a buffer
createBuffer :: Memory -> Word64 -> StateT SchedState IO Buffer
createBuffer mem sz = do
  buf <- lift $ allocBuffer mem sz
  registerBuffer mem buf
  lift $ return buf

-- | Release a buffer
releaseBuffer :: Buffer -> StateT SchedState IO ()
releaseBuffer buf = do
  unregisterBuffer buf
  lift $ freeBuffer buf

-- | Register a new buffer
registerBuffer :: Memory -> Buffer -> StateT SchedState IO ()
registerBuffer mem buf = modify (buffers ^%= modBuffer)
  where
    modBuffer = alter f mem
    f (Just x) = Just (x ++ [buf])
    f Nothing  = Just [buf]

-- | Unregister a buffer
unregisterBuffer :: Buffer -> StateT SchedState IO ()
unregisterBuffer buf = modify (buffers ^%= modBuffer)
  where
    mem = getBufferMemory buf
    modBuffer :: Map Memory [Buffer] -> Map Memory [Buffer]
    modBuffer = alter (fmap $ List.delete buf) mem

-- | Start a new asynchronous transfer
submitTransfer :: Transfer -> StateT SchedState IO ()
submitTransfer transfer@(Transfer link _ _) = do
  lift $ if not (checkTransfer transfer) then error "Invalid transfer" else return ()
  registerActiveTransfer transfer
  ch <- gets $ \x -> (linkChans ^$ x) ! link
  lift $ writeChan ch transfer

-- | Register an active transfer
registerActiveTransfer :: Transfer -> StateT SchedState IO ()
registerActiveTransfer transfer = modify(activeTransfers ^%= (++) [transfer])

