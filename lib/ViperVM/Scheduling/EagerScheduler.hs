module ViperVM.Scheduling.EagerScheduler (
  eagerScheduler
  ) where

import ViperVM.Kernel
import ViperVM.KernelSet
import ViperVM.RuntimeInternal
import ViperVM.Task
import ViperVM.Internals.Logging
import ViperVM.Internals.Structures(Scheduler, Message(..), scheduledTasks, voidR)

import Control.Monad.State
import Data.Lens.Lazy
import Data.List (sortBy)
import Data.Map (toList,insertWith)
import Data.Maybe (listToMaybe,fromJust)
import Text.Printf

-- | Schedule task on the processor able to handle it which has the smallesst number of assigned tasks
eagerScheduler :: Scheduler

eagerScheduler (TaskSubmitted task) = do
  let Task (KernelSet _ ks) _ = task

  procTasks <- gets (scheduledTasks ^$)

  -- Sort processors with the number of assigned tasks
  let sorted = fmap fst $ sortBy (\(_,x) (_,y) -> compare (length x) (length y)) $ toList procTasks

  -- Filter invalid processors and select the first valid one
  valid <- lift $ filterM (\p -> 
    liftM (any id) $ mapM (canExecute p) ks) sorted

  -- TODO: support the case where no processor can execute the task
  let proc = fromJust $ listToMaybe valid

  let newProcTasks = insertWith (++) proc [task] procTasks
  modify (scheduledTasks ^= newProcTasks)

  postMessageR $ TaskScheduled task proc
  logInfoR $ printf "[Eager Scheduler] Scheduling task %s on %s" (show task) (show proc)


eagerScheduler _ = voidR
