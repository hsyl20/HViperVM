module ViperVM.Scheduling.TaskRequestManager where

import ViperVM.RuntimeInternal

taskRequestManager :: Scheduler

taskRequestManager (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task
  storeTaskRequestsR task requests

taskRequestManager _ = voidR
