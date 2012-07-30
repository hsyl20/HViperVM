module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import ViperVM.RuntimeInternal

taskManagerScheduler :: Scheduler

taskManagerScheduler (SubmitTask task r) = do
  addPendingTask task r

taskManagerScheduler _ = voidR
