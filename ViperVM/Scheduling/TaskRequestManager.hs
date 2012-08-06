{-# LANGUAGE TupleSections #-}

module ViperVM.Scheduling.TaskRequestManager where

import ViperVM.RuntimeInternal
import Data.Maybe (catMaybes)
import Data.Functor ( (<$>) )
import Data.Foldable (traverse_)

taskRequestManager :: Scheduler

taskRequestManager (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task
  storeTaskRequestsR task requests

-- When a kernel is compiled
taskRequestManager (KernelCompiled k procs cks) = do
  traverse_ (uncurry $ storeCompiledKernelR k) $ catMaybes $ (\(ck,p) -> fmap (,p) ck) <$> zip cks procs
  updateCompilationRequestsR
  

taskRequestManager _ = voidR
