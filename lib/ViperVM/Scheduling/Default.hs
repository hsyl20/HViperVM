module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.Compiler
import ViperVM.Logging.Logger
import ViperVM.Internals.Structures (Scheduler)
import ViperVM.Scheduling.Composed
import ViperVM.Scheduling.DataManager
import ViperVM.Scheduling.EagerKernelCompiler
import ViperVM.Scheduling.EagerScheduler
import ViperVM.Scheduling.MemoryManager
import ViperVM.Scheduling.TaskManager
import ViperVM.Scheduling.TaskRequestManager

-- | Default scheduler uses:
--
--     * A single thread compiler
--
--     * An eager kernel compiler
--
--     * An eager scheduler
--
--     * Default data manager, task manager and task request manager
--
-- [@logger@] Logger to use
defaultScheduler :: Logger -> IO Scheduler

defaultScheduler logger = do
  compiler <- initSingleCompiler logger

  return $ composedScheduler [
    eagerKernelCompiler compiler,
    eagerScheduler,
    memoryManagerScheduler,
    dataManagerScheduler,
    taskManagerScheduler,
    taskRequestManager]
  
