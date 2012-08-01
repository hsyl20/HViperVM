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

taskManagerScheduler (AppTaskSubmit ks@(KernelSet ki _) ds r) = do
  
  -- Make kernel parameters from inputs:
  --   - data accessed in read-only mode
  --   - data accessed in read-write mode
  --   - data that must be allocated
  let kps = makeParameters ki ds

  -- Create task parameters
  params <- traverse kpToTp kps

  let task = Task ks params
  let datas = map paramToData params

  -- Store task in state
  modify (submittedTasks ^%= (:) task)

  -- Indicate that a task has been submitted
  postMessageR $ TaskSubmitted task

  -- Return result data
  let result = makeResult ki datas
  setEventR r result

taskManagerScheduler _ = voidR
