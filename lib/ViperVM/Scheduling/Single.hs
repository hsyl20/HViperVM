{-# LANGUAGE LambdaCase #-}
module ViperVM.Scheduling.Single (
      singleScheduler, initSingleScheduler
   ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void,forever,forM)
import Data.Foldable (forM_)

-- | Scheduler using a single processor
singleScheduler :: Processor -> Scheduler
singleScheduler proc = Scheduler (initSingleScheduler proc)


initSingleScheduler :: Processor -> SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initSingleScheduler proc som km ch = void $ forkIO (singleThread proc som km ch)


singleThread :: Processor -> SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
singleThread proc som km ch = forever (parseMsg =<< atomically (readTChan ch))
   where
      om = objectManager som
      mem = head (processorMemories proc)

      parseMsg :: SchedMsg -> IO ()
      parseMsg msg = case msg of 
         SchedExec k args res -> do
            
            ensureCompiledFor km k proc

            putStrLn ("[Single] Executing " ++ show k ++ " with params " ++ show args)

            -- Move input data in memory
            let argModes = args `zip` lockModes k
            args' <- forM argModes $ \(arg,mode) -> case mode of
               ReadOnly -> ensureInMemory som mem arg 
               ReadWrite -> allocateInstance som arg mem

            -- Execute kernel
            executeObjectKernel om proc k args'

            -- Associate output parameters
            forM_ (argModes `zip` args') $ \((arg,mode),arg') -> case mode of
               ReadOnly -> return ()
               ReadWrite -> atomically (attachObject arg arg')

            -- Return result
            atomically (writeTVar res SchedSuccess)
