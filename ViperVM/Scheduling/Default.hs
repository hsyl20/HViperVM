module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.Compiler
import ViperVM.Logging.Logger
import ViperVM.RuntimeInternal
import ViperVM.Scheduling.Composed
import ViperVM.Scheduling.DataManager
import ViperVM.Scheduling.EagerKernelCompiler
import ViperVM.Scheduling.EagerScheduler
import ViperVM.Scheduling.TaskManager
import ViperVM.Scheduling.TaskRequestManager

defaultScheduler :: Logger -> IO Scheduler

defaultScheduler logger = do
  compiler <- initSingleCompiler logger

  return $ composedScheduler [
    eagerKernelCompiler compiler,
    eagerScheduler,
    dataManagerScheduler,
    taskManagerScheduler,
    taskRequestManager]
  
