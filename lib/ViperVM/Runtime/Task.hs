module ViperVM.Runtime.Task where

import ViperVM.Runtime.Nodes
import qualified ViperVM.STM.TSet as TSet
import Control.Concurrent.STM

-- | Create a task instance node
createTaskInstance :: Task -> [DataInstance] -> Processor -> STM TaskInstance
createTaskInstance t dis p = do
   let ti = TaskInstance t dis p
   TSet.insert ti (taskInstances t)
   TSet.insert ti (procTaskInstances p)
   return ti
