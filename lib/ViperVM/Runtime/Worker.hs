module ViperVM.Runtime.Worker where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Applicative
import qualified ViperVM.Platform as Pf
import ViperVM.Runtime.Nodes
import ViperVM.Runtime.Data
import ViperVM.STM.TSet
import qualified ViperVM.STM.TSet as TSet
import Data.Sequence
import Data.Maybe
import qualified Data.Set as Set
import Data.Set
import qualified Data.Sequence as Seq

data Worker = Worker {
   workerProc :: Processor,
   pending :: TVar (Seq Task),
   completed :: TSet TaskInstance,
   failed :: TSet Task
}

-- | A processor worker execute tasks in the pending list
createWorker :: Processor -> IO Worker
createWorker proc = do
   w <- atomically (Worker proc <$> newTVar Seq.empty <*> TSet.empty <*> TSet.empty)
   void $ forkOS $ workerThread w
   return w


workerThread :: Worker -> IO ()
workerThread w = do
   let proc = workerProc w 
       instances = procTaskInstances proc

   forever $ do
      atomically $ do
         mems <- readTVar (procMemories proc)

         -- Get next task or retry until a new task is submitted
         mt <- popNextTask w
         when (isNothing mt) retry
         let Just t = mt
            
         -- Retrieve data instances for input parameters
         insts <- forM (inputParams t) (dataInstancesInMemories mems)

         -- Transfer input data if they are available and if required, or retry

         return ()


-- | Pop the next pending task
popNextTask :: Worker -> STM (Maybe Task)
popNextTask w = do
   ts <- readTVar (pending w)
   case viewr ts of
      EmptyR  -> return Nothing
      vs :> v -> do
         writeTVar (pending w) vs
         return (Just v)

-- | Push a task to the worker
pushTask :: Worker -> Task -> STM ()
pushTask w t = do
   ts <- readTVar (pending w)
   writeTVar (pending w) (t <| ts)


-- | Return the number of pending tasks
pendingTaskCount :: Worker -> STM Int
pendingTaskCount w = Seq.length <$> readTVar (pending w)

-- | Indicate kernels that a worker can excute for a task
workerExecutableTaskKernels :: Task -> Worker -> STM (Set Kernel)
workerExecutableTaskKernels t w = do
   let proc = procPeer (workerProc w)
   
   metaKs <- readTVar $ metaKernels (metaKernel t)
   return $ Set.filter (\k -> Pf.canExecute proc (kernelPeer k)) metaKs
   
