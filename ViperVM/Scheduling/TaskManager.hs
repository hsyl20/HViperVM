module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import Control.Monad.State
import Data.Lens.Lazy
import Data.Traversable
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.RuntimeInternal
import ViperVM.Task

taskManagerScheduler :: Scheduler

taskManagerScheduler (SubmitTask task r) = do
  
  let Task (KernelSet ki _) ds = task
  let params = makeParameters ki ds

  datas <- traverse paramToData params

  modify (submittedTasks ^%= (:) (task,datas))

  let result = makeResult ki datas

  setEventR r result

taskManagerScheduler _ = voidR
