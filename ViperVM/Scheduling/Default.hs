module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.RuntimeInternal
import ViperVM.Scheduling.Composed
import ViperVM.Scheduling.StartStop
import ViperVM.Scheduling.DataManager
import ViperVM.Scheduling.TaskManager
import ViperVM.Scheduling.KernelManager

defaultScheduler :: Scheduler

defaultScheduler = composedScheduler [
  startStopScheduler,
  dataManagerScheduler,
  taskManagerScheduler,
  kernelManagerScheduler]
  
