module ViperVM.Scheduling.Executer where

import ViperVM.Data
import ViperVM.KernelSet
import ViperVM.Platform
import ViperVM.RuntimeInternal
import ViperVM.Task

import Data.List (intersect)
import Data.Maybe
import Data.Set (fromList)
import Data.Traversable


executer :: Scheduler

executer (TaskScheduled task proc) = do
  let Task (KernelSet ki ks) params = task

  -- Check that input data have been computed (i.e. have at least one instance)
  let inputs = inputDatas params
  inputInstances <- traverse getInstancesR inputs
  let computeReqs = map RequestComputation $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstances inputs

  -- Check that there is an instance of each parameter available in memory
  let mems = attachedMemories proc
  let inputInstanceMemories = map (intersect mems . map getDataInstanceMemory) inputInstances
  let transferReqs = map (RequestTransfer mems) $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstanceMemories inputs

  -- Check that a kernel for the given proc has been compiled
  cced <- traverse (getCompiledKernelR proc) ks
  let compileReqs = if null (catMaybes cced) then [RequestCompilation ks proc] else []

  -- Check read-write data (that may need to be duplicated)
  -- TODO

  -- Check allocated output data
  -- TODO
  
  -- Store requests
  let requests = fromList $ computeReqs ++ transferReqs ++ compileReqs
  registerTaskRequestsR task requests

executer (Request r) = do
  putStrLnR $ show r

executer _ = voidR
