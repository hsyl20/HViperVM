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

  -- Make kernel parameters from inputs:
  --   - data accessed in read-only mode
  --   - data accessed in read-write mode
  --   - data that must be allocated
  let params = makeParameters ki ds

  -- Retrieve or create data for each parameter
  datas <- traverse paramToData params

  -- Store task and data in state
  modify (submittedTasks ^%= (:) (task,datas))

  -- Return result data
  let result = makeResult ki datas
  setEventR r result

taskManagerScheduler _ = voidR
