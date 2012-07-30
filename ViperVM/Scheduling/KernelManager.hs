module ViperVM.Scheduling.KernelManager (
  kernelManagerScheduler
  ) where

import ViperVM.RuntimeInternal

kernelManagerScheduler :: Scheduler

kernelManagerScheduler (RegisterKernel k ev) = do
  ck <- compileKernelR k
  setEventR ev ck

kernelManagerScheduler _ = voidR
