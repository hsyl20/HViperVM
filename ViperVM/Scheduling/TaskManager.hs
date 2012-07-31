module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import ViperVM.RuntimeInternal
import Control.Monad.State
import Data.Lens.Lazy

taskManagerScheduler :: Scheduler

taskManagerScheduler (SubmitTask task) = do
  modify (submittedTasks ^%= (:) task)

taskManagerScheduler _ = voidR
