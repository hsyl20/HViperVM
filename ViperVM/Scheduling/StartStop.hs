module ViperVM.Scheduling.StartStop (
  startStopScheduler
  ) where

import ViperVM.RuntimeInternal
import ViperVM.Event
import Control.Monad.State

startStopScheduler :: Scheduler

startStopScheduler (Quit ev) = do
  logInfo "Stopping the runtime..."
  logInfo "This log will now be closed"
  shutdownLogger
  lift $ setEvent ev ()

startStopScheduler _ = voidR
