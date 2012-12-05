module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import Control.Monad.State
import Data.Lens.Lazy
import Data.Map (insert)
import Data.Foldable
import Data.Traversable
import Text.Printf

import ViperVM.Internals.Logging
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.Internals.Structures (Scheduler, Message(..), submittedTasks, dataTasks, voidR, postMessageR)
import ViperVM.RuntimeInternal (kpToTp, setEventR)
import ViperVM.Task

taskManagerScheduler :: Scheduler

-- | When an application submits a task, its resulting data handles are
-- generated and returned and the task is stored to be executed
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

  -- Store data->task association
  traverse_ (\d -> modify (dataTasks ^%= insert d task)) datas

  -- Indicate that a task has been submitted
  postMessageR $ TaskSubmitted task

  -- Return result data
  let result = makeResult ki datas
  setEventR r result

-- | When a task is ready to be executed on a proc, a kernel is selected
-- and the asynchronous execution is planned
taskManagerScheduler (TaskReady task@(Task ks params) proc) = do
  logInfoR $ printf "Task %s ready to be executed! (TODO)" (show task)
  -- Select kernel
  let k = head ks
  TODO -- call execute in Executer
  postMessageR $ TaskComplete task


taskManagerScheduler _ = voidR