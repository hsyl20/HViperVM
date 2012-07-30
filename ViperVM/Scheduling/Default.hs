module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.RuntimeInternal
import ViperVM.Logging.Logger
import ViperVM.Compiler
import ViperVM.Scheduling.Composed
import ViperVM.Scheduling.DataManager
import ViperVM.Scheduling.TaskManager
import ViperVM.Scheduling.EagerKernelCompiler

defaultScheduler :: Logger -> IO Scheduler

defaultScheduler logger = do
  compiler <- initSingleCompiler logger

  return $ composedScheduler [
    eagerKernelCompiler compiler,
    dataManagerScheduler,
    taskManagerScheduler]
  
