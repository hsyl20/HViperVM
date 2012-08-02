module ViperVM.Scheduling.Executer where

import ViperVM.RuntimeInternal

executer :: Scheduler

executer (TaskScheduled task proc) = do
  requests <- determineTaskRequests proc task

  -- Store requests
  registerTaskRequestsR task requests

executer (Request r) = do
  putStrLnR $ show r

executer _ = voidR
