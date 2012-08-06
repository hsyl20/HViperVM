module ViperVM.Scheduling.Composed where

import ViperVM.Internals.Structures (Scheduler)
import Control.Monad.State

-- | Compose a list of schedulers (executed from left to right)
composedScheduler :: [Scheduler] -> Scheduler
composedScheduler [] _ = lift $ return ()
composedScheduler (a:as) msg = do
  a msg
  composedScheduler as msg
