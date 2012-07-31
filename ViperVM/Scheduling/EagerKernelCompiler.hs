module ViperVM.Scheduling.EagerKernelCompiler (
  eagerKernelCompiler
  ) where

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

eagerKernelCompiler compiler (SubmitTask task) = do
  let Task (KernelSet _ ks) _ _ _ = task
  procs <- getProcessorsR
  channel <- getChannelR

  let cc k = compile compiler procs k (callback channel k procs)
    
  _ <- lift $ traverse cc ks

  return ()

  where
    callback channel k procs cced = do
      writeChan channel $ KernelCompiled k procs cced

eagerKernelCompiler compiler (Quit _) = do
  logInfoR "Waiting for compilations to terminate..."
  v <- lift $ newEmptyMVar
  lift $ shutdown compiler (putMVar v ())
  lift $ readMVar v
  logInfoR "Compilations completed."

eagerKernelCompiler _ _ = voidR
