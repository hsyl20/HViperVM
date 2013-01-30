module ViperVM.DataGraph (
   DataGraph, GNode(..), TaskState(..), TaskInstance(..), DataTransfer(..),
) where

import ViperVM.Graph
import ViperVM.Data
import ViperVM.Task
import ViperVM.Platform

import Data.Set
import Control.Concurrent.STM

type DataGraph = Graph GNode

data GNode =
   DataNode {
      desc :: DataDesc,
      instances :: TVar (Set DataInstance),
      transfers :: TVar (Set DataTransfer)
   } |
   TaskNode {
      task :: Task,
      state :: TVar TaskState,
      candidates :: TVar (Set Processor)
   }


{- | Task state
   A task can be executed by different processors simultaneously
   for different reasons using different data instances
-}
data TaskState = Pending | 
                 Executing [TaskInstance]

-- | An instance of a task being executed
data TaskInstance = TaskInstance Processor [DataInstance]

-- | A data transfer
data DataTransfer = DataTransfer Memory Memory

