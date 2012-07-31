module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import ViperVM.RuntimeInternal

taskManagerScheduler :: Scheduler

taskManagerScheduler (SubmitTask task) = do
  addPendingTask task

taskManagerScheduler _ = voidR
