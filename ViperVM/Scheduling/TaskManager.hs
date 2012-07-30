module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import ViperVM.RuntimeInternal

taskManagerScheduler :: Scheduler

taskManagerScheduler (SubmitTask kernel params r) = do
  setEventR r ()

taskManagerScheduler _ = voidR
