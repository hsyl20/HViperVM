module ViperVM.Runtime.DefaultScheduler (
   createRuntime,
   module X
) where

import ViperVM.Runtime as X
import ViperVM.Runtime.GenericScheduler
import ViperVM.Runtime.Worker
import qualified ViperVM.Platform as Pf
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import qualified Data.Set as Set

createRuntime :: Pf.Platform -> IO Runtime
createRuntime pf = do
   compiler <- startCompilerThread

   r <- createDefaultRuntime pf
   
   policy <- eagerPolicy (processors r)
   sched <- createGenericScheduler policy

   void $ forkIO $ forever $ atomically $ do
      ev <- readTChan (events r)
      case ev of
         NotifyKernelRegister ki k -> compiler (k, processors r)
         NotifyTaskSubmit t -> sched (Submitted t)
         NotifyMapData _ -> return ()
         NotifyWaitData ds -> sched (Required ds)

   return r


-- | Schedule tasks on the first device available to execute them that is free
eagerPolicy :: [Processor] -> IO (SchedEvent -> STM ())
eagerPolicy procs = do
   workers <- forM procs createWorker

   let f (Submitted t) = do
         wks <- mapM (workerExecutableTaskKernels t) workers
         w <- select (workers `zip` wks)
         pushTask w t

   return f
   where

      select [] = retry
      select ((w,ks):xs) = do
         tc <- pendingTaskCount w
         if tc == 0 && not (Set.null ks) 
            then return w
            else select xs

