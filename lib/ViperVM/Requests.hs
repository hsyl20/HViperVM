{-# LANGUAGE TupleSections #-}

module ViperVM.Requests (
  updateCompilationRequestsR, updateAllocationRequestsR, updateTransferRequestsR,
  storeTaskRequestsR, registerActiveRequestR,determineTaskRequests,
  ) where

import ViperVM.Structures
import ViperVM.Task
import ViperVM.KernelSet
import ViperVM.Platform

import Control.Monad.State
import Control.Applicative

import Data.Foldable (traverse_)
import Data.Lens.Lazy
import Data.List (intersect)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Traversable (traverse)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Detect ready tasks
detectReadyTasksR :: R ()
detectReadyTasksR = do
  tasks <- gets(Map.keys . Map.filter Set.null . getL taskRequests)
  traverse_ (postMessageR . TaskReady) tasks

-- | Filter stored requests
filterRequestsR :: (TaskRequest -> Bool) -> R ()
filterRequestsR f = do
  modify(taskRequests ^%= Map.map (Set.filter f))
  modify(requestTasks ^%= Map.filterWithKey (\k _ -> f k))
  modify(activeRequests ^%= Set.filter f)

-- | Filter requests and detect ready tasks
updateRequestsR :: (TaskRequest -> Bool) -> R ()
updateRequestsR f = do
  filterRequestsR f
  detectReadyTasksR

-- | Remove compilation requests that have been fulfilled
updateCompilationRequestsR :: R ()
updateCompilationRequestsR = do
   return () --FIXME
    {-cks <- compiledKernelsR
    updateRequestsR (f cks)
  where
    f cks (RequestCompilation ks p) = all (\k -> isNothing $ getCompiledKernel p k cks) ks
    f _ _ = True-}
   
-- | Remove allocation requests that have been fulfilled
updateAllocationRequestsR :: R ()
updateAllocationRequestsR = do
    instances <- getInvalidDataInstancesR
    instanceMem <- traverse (traverse getDataInstanceMemoryR) instances
    updateRequestsR (f instanceMem)
  where
    f instMem (RequestAllocation mems d) = null $ intersect mems $ Map.findWithDefault [] d instMem
    f _ _ = True

-- | Remove transfer requests that have been fulfilled
updateTransferRequestsR :: R ()
updateTransferRequestsR = do
    ds <- datasR
    dataMems <- traverse (instancesR >=> traverse getDataInstanceMemoryR) ds
    let dataInsts = Map.fromList $ ds `zip` dataMems
    updateRequestsR (f dataInsts)
  where
    f dataMem (RequestTransfer mems d) = null $ intersect mems $ Map.findWithDefault [] d dataMem
    f _ _ = True

-- | Register a request being fulfilled
registerActiveRequestR :: TaskRequest -> R ()
registerActiveRequestR req = modify(activeRequests ^%= Set.insert req)

-- | Store task requests
storeTaskRequestsR :: Task -> Set TaskRequest -> R ()
storeTaskRequestsR t reqs = do
  modify(taskRequests ^%= Map.alter (const $ Just reqs) t)
  traverse_ (storeRequestTaskR t) reqs
  postMessageR RequestsStored
  
-- | Store a request if it doesn't already exist and associate a task to it
storeRequestTaskR :: Task -> TaskRequest -> R ()
storeRequestTaskR t r = do
  modify(requestTasks ^%= Map.alter f r)
  where
    f (Just ts) = Just (t:ts)
    f Nothing = Just [t]


-- | Determine requests that need to be fulfilled for a task to be scheduled on
-- a given processor
determineTaskRequests :: Processor -> Task -> R (Set TaskRequest)
determineTaskRequests proc task = do
  let Task (KernelSet _ ks) params = task

  -- Check that input data have been computed (i.e. have at least one instance)
  let inputs = roDatas params

  inputInstances <- traverse instancesR inputs
  let computeReqs = map RequestComputation $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstances inputs

  -- Check that there is an instance of each parameter available in memory
  let mems = processorMemories proc
  inputInstanceMemories <- traverse (\x -> intersect mems <$> traverse getDataInstanceMemoryR x) inputInstances
  let transferReqs = map (RequestTransfer mems) $ catMaybes $ zipWith (\x y -> if null x then Just y else Nothing) inputInstanceMemories inputs

  -- Check that a kernel for the given proc has been compiled
  cced <- traverse (getCompiledKernelR proc) ks
  let compileReqs = if null (catMaybes cced) then [RequestCompilation ks proc] else []

  -- Check allocated output data
  let outputs = woDatas params
  isAllocatedOutput <- traverse (flip isDataAllocatedAnyR mems) outputs
  let allocReqs = map (RequestAllocation mems) $ catMaybes $ zipWith (\x y -> if x then Just y else Nothing) isAllocatedOutput outputs

  -- Checks for read-write data: we need to detect if an instance of the input
  -- data can be detached
  let rwInputs = rwInputDatas params
  detachableInstances <- traverse (flip getDetachableInstancesAnyR mems) rwInputs
  let duplicateReqs = map (RequestDuplication mems) $ catMaybes $ zipWith (\x y -> if Set.null x then Just y else Nothing) detachableInstances rwInputs

  let requests = Set.fromList $ computeReqs ++ transferReqs ++ compileReqs ++ allocReqs ++ duplicateReqs
  return requests

