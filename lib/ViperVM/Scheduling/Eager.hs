module ViperVM.Scheduling.Eager (
      eagerScheduler
   ) where

import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.SharedObject
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler

import Control.Concurrent.STM

-- | Scheduler with a greedy strategy
eagerScheduler :: Scheduler
eagerScheduler = Scheduler initEagerScheduler

initEagerScheduler :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initEagerScheduler som km ch = do
   putStrLn ""
