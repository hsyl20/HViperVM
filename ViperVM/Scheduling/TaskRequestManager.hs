{-# LANGUAGE TupleSections #-}

module ViperVM.Scheduling.TaskRequestManager where

import ViperVM.RuntimeInternal

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Functor ( (<$>) )
import Data.Lens.Lazy
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.List as List

taskRequestManager :: Scheduler

taskRequestManager (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task
  storeTaskRequestsR task requests

taskRequestManager (KernelCompiled k procs cks) = do
  traverse_ (uncurry $ storeCompiledKernelR k) $ catMaybes $ (\(ck,p) -> fmap (,p) ck) <$> zip cks procs
  updateCompilationRequestsR
  
taskRequestManager (DataAllocated d di) = do
  modify (invalidDataInstances ^%= Map.insertWith (++) d [di])
  updateAllocationRequestsR

taskRequestManager (DataTransfered d di) = do
  modify (invalidDataInstances ^%= Map.adjust (List.delete di) d)
  modify (datas ^%= Map.insertWith (++) d [di])
  updateTransferRequestsR

taskRequestManager _ = voidR
