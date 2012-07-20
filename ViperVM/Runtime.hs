{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Runtime (
  startRuntime,
  stopRuntime,
  mapVector
  ) where

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Transfer
import ViperVM.Task
import ViperVM.Data
import ViperVM.View
import ViperVM.Kernel

import Control.Concurrent
import Control.Applicative ( (<$>), liftA2 )
import Control.Monad.State
import Data.Map
import Data.Word
import qualified Data.List as List
import Data.Lens.Lazy
import Data.Lens.Template
import Foreign.Ptr

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
  _channel :: Chan Message,                 -- ^ Channel to communicate with the runtime
  _platform :: Platform,                    -- ^ Platform used by the runtime
  _buffers :: Map Memory [Buffer],          -- ^ Buffers in each memory
  _activeTransfers :: [Transfer],           -- ^ Current data transfers
  _linkChans :: Map Link (Chan Transfer),   -- ^ Channels to communicate with link threads
  _datas :: Map Data [DataInstance],        -- ^ Registered data
  _dataCounter :: Word                      -- ^ Data counter (used to set data ID)
}

$( makeLens ''RuntimeState ) 

type R a = StateT RuntimeState IO a

data Runtime = Runtime (Chan Message)

-- | Starts the runtime on the given platform
startRuntime :: Platform -> IO Runtime
startRuntime pf = do
  (st,ch) <- initRuntimeState pf
  _ <- forkIO $ do
    _ <- execStateT runtime st
    return ()
  return $ Runtime ch

-- | Stop the given runtime
stopRuntime :: Runtime -> IO ()
stopRuntime (Runtime ch) = writeChan ch Quit 

-- | Map a Vector
mapVector :: Runtime -> VectorDesc -> Ptr () -> IO Data
mapVector (Runtime ch) desc ptr = do
  v <- newEmptyMVar
  writeChan ch $ MapVector desc ptr v
  takeMVar v

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit Quit = True
isQuit _ = False

-- | Initialize runtime state
initRuntimeState :: Platform -> IO (RuntimeState,Chan Message)
initRuntimeState pf = do
  ch <- newChan
  lkChans <- fromList <$> liftA2 zip (return $ links pf) (replicateM (length $ links pf) newChan)
  let st = RuntimeState {
    _channel = ch,
    _platform = pf,
    _buffers = empty,
    _activeTransfers = [],
    _linkChans = lkChans,
    _datas = empty,
    _dataCounter = 0
  }
  return (st,ch)

-- | Launch the runtime on the given platform. Communication is done through the channel
runtime :: R ()
runtime = do
  ch <- gets (channel ^$)
  msg <- lift $ readChan ch
  case msg of
    TaskSubmit t -> taskSubmit t
    KernelComplete k -> kernelComplete k
    TransferComplete t -> transferComplete t
    MapVector desc ptr r -> mapVectorInternal desc ptr r
    DataRelease d -> dataRelease d
    Quit -> lift $ return ()
  unless (isQuit msg) runtime

  where

  taskSubmit :: Task -> R ()
  taskSubmit t = undefined

  kernelComplete :: Kernel -> R ()
  kernelComplete k = undefined

  transferComplete :: Transfer -> R ()
  transferComplete t = undefined

  mapVectorInternal :: VectorDesc -> Ptr () -> MVar Data -> R ()
  mapVectorInternal desc@(VectorDesc prim n) ptr r = do
    let sz = n * primitiveSize prim
    buf <- lift . return $ HostBuffer sz ptr
    registerBuffer HostMemory buf
    let view = View1D buf 0 sz
    let di = Vector desc view
    d <- newData
    registerDataInstance d di
    lift $ putMVar r d

  dataRelease :: Data -> R ()
  dataRelease d = undefined

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
  ch <- gets $ \x -> (linkChans ^$ x) ! link
  lift $ writeChan ch transfer

-- | Register an active transfer
registerActiveTransfer :: Transfer -> R ()
registerActiveTransfer transfer = modify(activeTransfers ^%= (++) [transfer])

