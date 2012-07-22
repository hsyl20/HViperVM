module ViperVM.Scheduling.KernelManager (
  kernelManagerScheduler
  ) where

import ViperVM.RuntimeInternal

import Control.Concurrent
import Control.Monad.State

kernelManagerScheduler :: Scheduler

kernelManagerScheduler (RegisterKernel k v) = do
  ck <- compileKernelR k
  lift $ putMVar v ck
  return ()

kernelManagerScheduler _ = voidR
