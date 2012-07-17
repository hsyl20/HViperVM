module ViperVM.Scheduler where

import ViperVM.Platform

import Control.Concurrent.Chan
import Control.Monad.State

data Message = TaskSubmit () | TaskComplete () | TransferComplete ()

data SchedState = SchedState ()

scheduler :: Chan Message -> Platform -> StateT SchedState IO ()
scheduler ch p = do
  scheduler ch p
