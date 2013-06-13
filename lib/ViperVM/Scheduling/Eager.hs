module ViperVM.Scheduling.Eager (
      eagerScheduler
   ) where

import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler
import ViperVM.Platform.Platform (processors)
import ViperVM.Scheduling.Single

import Control.Concurrent.STM

import Data.Foldable (forM_)

-- | Scheduler with a greedy strategy
eagerScheduler :: Scheduler
eagerScheduler = Scheduler initEagerScheduler


initEagerScheduler :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initEagerScheduler som km ch = do
   let procs = processors (getSharedObjectManagerPlatform som)

   -- Create one single scheduler per proc
   -- All workers peek into the same chan
   forM_ procs $ \proc -> do
      initSingleScheduler proc som km ch
