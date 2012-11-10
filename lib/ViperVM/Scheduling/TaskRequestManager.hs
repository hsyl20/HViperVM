{-# LANGUAGE TupleSections #-}

module ViperVM.Scheduling.TaskRequestManager where

import ViperVM.Internals.Requests
import ViperVM.Internals.Structures

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Functor ( (<$>) )
import Data.Lens.Lazy
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.List as List

-- | Maintain the list of requests that need to be fulfilled to execute a scheduled task
taskRequestManager :: Scheduler

-- When a task is scheduled on a proc, we determine its initial request set
taskRequestManager (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task
  storeTaskRequestsR task requests

-- When a kernel has been compiled for several procs, we store the compiled kernels
-- and we remove compilation requests that have been fulfilled
taskRequestManager (KernelCompiled k procs cks) = do
  traverse_ (uncurry $ storeCompiledKernelR k) $ catMaybes $ (\(ck,p) -> fmap (,p) ck) <$> zip cks procs
  updateCompilationRequestsR
  
-- When an invalid data instance is allocated, we store it and we update allocation requests
taskRequestManager (DataAllocated d di) = do
  modify (invalidDataInstances ^%= Map.insertWith (++) d [di])
  updateAllocationRequestsR

-- When a transfer has completed to initialize a data, we indicate that the latter is
-- no longer invalid, we store the new data and we update transfer requests
taskRequestManager (DataTransfered d di) = do
  modify (invalidDataInstances ^%= Map.adjust (List.delete di) d)
  modify (datas ^%= Map.insertWith (++) d [di])
  updateTransferRequestsR

taskRequestManager _ = voidR
