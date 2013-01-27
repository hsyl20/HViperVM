module ViperVM.Scheduling.TaskManager (
  taskManagerScheduler
  ) where

import Control.Monad.State
import Data.Lens.Lazy
import Data.Map (insert)
import Data.Foldable (traverse_)
import Data.Traversable
import Text.Printf

import ViperVM.Logging
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.Structures
import ViperVM.Task

taskManagerScheduler :: Scheduler

-- | When an application submits a task, its resulting data handles are
-- generated and returned and the task is stored to be executed
taskManagerScheduler (AppTaskSubmit ks@(KernelSet ki _) ds r) = do
  
  dataMgr <- getDataManagerR

  -- Make kernel parameters from inputs:
  --   - data accessed in read-only mode
  --   - data accessed in read-write mode
  --   - data that must be allocated
  let kps = makeParameters ki dataMgr ds

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
taskManagerScheduler (TaskReady task@(Task ks params)) = do
  logInfoR $ printf "Task %s ready to be executed! (TODO)" (show task)

{- FIXME

  -- Select kernel
  let (KernelSet _ (k:_)) = ks

  -- Select proc
  proc <- (head . toList. filterWithKey (\k v -> elem task v)) <$> getScheduledTasksR

  let conf = configure k params
  --TODO -- call execute in Executer
  chan <- getChannelR
  lift $ execute proc k conf $ do
    writeChan chan $ TaskComplete task -}


taskManagerScheduler _ = voidR
