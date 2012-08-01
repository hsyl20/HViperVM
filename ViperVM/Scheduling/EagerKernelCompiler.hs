{-# LANGUAGE TupleSections #-}

module ViperVM.Scheduling.EagerKernelCompiler (
  eagerKernelCompiler
  ) where

import Prelude hiding (lookup)
import ViperVM.Compiler
import ViperVM.KernelSet
import ViperVM.RuntimeInternal
import ViperVM.Task

import Data.Traversable
import Data.Maybe
import Data.Map (lookup,fromList,union,insert)
import Control.Concurrent.Chan
import Control.Concurrent
import Data.Functor ( (<$>) )
import Control.Monad.State (lift,gets,modify)
import Data.Lens.Lazy

-- | Schedule kernel compilation as soon as a task is submitted using the
-- provided compiler
eagerKernelCompiler :: Compiler -> Scheduler

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

eagerKernelCompiler _ (KernelCompiled k procs cks) = do
  let procCCed = fromList $ catMaybes $ (\(ck,p) -> fmap (p,) ck) <$> zip cks procs

  old <- gets (compiledKernels ^$)
  let new = fromMaybe procCCed (union procCCed <$> (lookup k old))

  modify (compiledKernels ^%= insert k new)


eagerKernelCompiler compiler (AppQuit _) = do
  logInfoR "Waiting for compilations to terminate..."
  v <- lift $ newEmptyMVar
  lift $ shutdown compiler (putMVar v ())
  lift $ readMVar v
  logInfoR "Compilations completed."

eagerKernelCompiler _ _ = voidR
