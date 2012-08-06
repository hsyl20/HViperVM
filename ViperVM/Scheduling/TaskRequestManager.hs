module ViperVM.Scheduling.TaskRequestManager where

import ViperVM.RuntimeInternal

taskRequestManager :: Scheduler

taskRequestManager (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task

  -- Store requests
  registerTaskRequestsR task requests

taskRequestManager (Request r) = do
  putStrLnR $ show r

taskRequestManager _ = voidR
