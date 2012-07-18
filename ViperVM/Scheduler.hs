{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Scheduler where

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Transfer

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Map
import Data.Word
import qualified Data.List as List
import Data.Lens.Lazy
import Data.Lens.Template

data Message = TaskSubmit () | TaskComplete () | TransferComplete ()

data SchedState = SchedState {
  _platform :: Platform,
  _buffers :: Map Memory [Buffer],
  _activeTransfers :: [Transfer],
  _linkChans :: Map Link (Chan Transfer)
}

$( makeLens ''SchedState ) 

scheduler :: Chan Message -> Platform -> StateT SchedState IO ()
scheduler ch p = do
  scheduler ch p

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

