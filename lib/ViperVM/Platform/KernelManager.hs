{-# LANGUAGE TupleSections, LambdaCase #-}

module ViperVM.Platform.KernelManager (
   KernelManager, KernelEvent, KernelExecution(..),
   createKernelManager, compileKernel,
   prepareKernelExecution, cancelKernelExecution, executeKernel
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Processor
import ViperVM.Platform.Kernel
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager
import ViperVM.Event

import Data.Map as Map

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forM,when)

data PrepareExecutionResult = PrepareExecSuccess | PrepareExecRegionAlreadyLocked
                              deriving (Eq)

data KernelExecution = KernelExecution {
      executableKernel :: Kernel,
      readOnlyRegions :: [(Buffer,Region)],
      readWriteRegions :: [(Buffer,Region)],
      kernelParameters :: [KernelParameter]
   }

type KernelEvent = Event ()

data KernelManager = KernelManager {
      regionManager :: RegionLockManager,
      procWorkers :: Map Processor (TChan (KernelExecution, KernelEvent))
   }

createKernelManager :: RegionLockManager -> IO KernelManager
createKernelManager rm = do
   let pf = getRegionManagerPlatform rm

   -- Create proc threads
   threads <- forM (processors pf) $ \proc -> do
      trs <- atomically newTChan
      _ <- forkOS (procThread proc trs)
      return (proc, trs)

   return $ KernelManager rm (fromList threads)


-- | Thread that perform kernel execution on a given proc
procThread :: Processor -> TChan (KernelExecution, KernelEvent) -> IO ()
procThread proc ch = do
   (exec,ev) <- atomically $ readTChan ch

   err <- execute proc (executableKernel exec) (kernelParameters exec)
   setEvent ev err

   procThread proc ch


-- | Compile a kernel for the given processors and return those for which it succeeded
compileKernel :: KernelManager -> Kernel -> [Processor] -> IO [Processor]
compileKernel _ k ps = compile k ps


-- | Prepare a kernel execution
--
-- Lock regions that will be used
prepareKernelExecution :: KernelManager -> KernelExecution -> STM PrepareExecutionResult
prepareKernelExecution km exec = do

   let rm = regionManager km

   r1 <- lockRegions rm ReadOnly (readOnlyRegions exec)
   r2 <- lockRegions rm ReadWrite (readWriteRegions exec)

   return $ if any (/= LockSuccess) (r1 ++ r2) 
               then PrepareExecRegionAlreadyLocked
               else PrepareExecSuccess


-- | Cancel a prepared kernel execution
--
-- Unlock regions
cancelKernelExecution :: KernelManager -> KernelExecution -> STM ()
cancelKernelExecution km exec = do

   let rm = regionManager km

   unlockRegions rm ReadOnly (readOnlyRegions exec)
   unlockRegions rm ReadWrite (readWriteRegions exec)


-- | Execute a kernel
executeKernel :: KernelManager -> Processor -> Kernel -> [(Buffer,Region)] -> [(Buffer,Region)] -> [KernelParameter] -> IO ()
executeKernel km proc k ro rw params = do

   let ch = procWorkers km Map.! proc
       exec = KernelExecution k ro rw params

   atomically $ do
      r <- prepareKernelExecution km exec
      when (r /= PrepareExecSuccess) retry

   ev <- newEvent
   atomically $ writeTChan ch (exec, ev)
   waitEvent ev
   atomically $ cancelKernelExecution km exec


