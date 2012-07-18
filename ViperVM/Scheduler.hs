{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Scheduler where

import ViperVM.Platform
import ViperVM.Buffer

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
  _buffers :: Map Memory [Buffer]
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

