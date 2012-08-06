module ViperVM.Scheduling.EagerKernelCompiler (
  eagerKernelCompiler
  ) where

import Prelude hiding (lookup)
import ViperVM.Compiler
import ViperVM.KernelSet
import ViperVM.RuntimeInternal
import ViperVM.Task

import Data.Traversable
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State (lift)

-- | Schedule kernel compilation as soon as a task is submitted using the
-- provided compiler
eagerKernelCompiler :: Compiler -> Scheduler

-- When a task is submitted
eagerKernelCompiler compiler (TaskSubmitted task) = do
  let Task (KernelSet _ ks) _ = task
  procs <- getProcessorsR
  channel <- getChannelR

  let cc k = compile compiler procs k (callback channel k procs)
    
  _ <- lift $ traverse cc ks

  return ()

  where
    callback channel k procs cced = do
      writeChan channel $ KernelCompiled k procs cced

-- When the application wants to shutdown the runtime
eagerKernelCompiler compiler (AppQuit _) = do
  logInfoR "Waiting for compilations to terminate..."
  v <- lift $ newEmptyMVar
  lift $ shutdown compiler (putMVar v ())
  lift $ readMVar v
  logInfoR "Compilations completed."

eagerKernelCompiler _ _ = voidR
