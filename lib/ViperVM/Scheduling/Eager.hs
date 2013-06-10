module ViperVM.Scheduling.Eager (
      eagerScheduler
   ) where

import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.SharedObject
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler

import Control.Concurrent.STM
import Control.Concurrent

import Control.Monad (void,forever)

-- | Scheduler with a greedy strategy
eagerScheduler :: Scheduler
eagerScheduler = Scheduler initEagerScheduler


initEagerScheduler :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initEagerScheduler som km ch = void $ forkIO (eagerThread som km ch)


eagerThread :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
eagerThread som km ch = do
   forever $ do
      msg <- atomically (readTChan ch)

      case msg of 
         SchedExec k args res -> do
            putStrLn ("[Eager] Execute " ++ show k ++ " with params " ++ show args)
            atomically (writeTVar res SchedSuccess)
