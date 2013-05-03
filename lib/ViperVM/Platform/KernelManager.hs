{-# LANGUAGE TupleSections #-}

module ViperVM.Platform.KernelManager (
   KernelManager, KernelEvent, KernelExecution(..),
   createKernelManager, registerKernel, compileKernel,
   prepareKernelExecution, cancelKernelExecution, executeKernel
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Processor
import ViperVM.Platform.Kernel
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.RegionManager
import ViperVM.Event
import ViperVM.STM.TMap as TMap

import Data.Map as Map
import Data.Maybe
import Data.Foldable (forM_)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forM)

data PrepareExecutionResult = PrepareExecSuccess | PrepareExecRegionAlreadyLocked

data KernelExecution = KernelExecution {
      executableKernel :: CompiledKernel,
      readOnlyRegions :: [(Buffer,Region)],
      readWriteRegions :: [(Buffer,Region)],
      kernelParameters :: [KernelParameter]
   }

type KernelEvent = Event ()

data KernelManager = KernelManager {
      regionManager :: RegionManager,
      procWorkers :: Map Processor (TChan (KernelExecution, KernelEvent)),
      registered :: TMap Kernel KernelInfo
   }

data KernelInfo = KernelInfo {
      compiled :: TMap Processor CompiledKernel
   }


createKernelManager :: RegionManager -> IO KernelManager
createKernelManager rm = do
   let pf = getPlatform rm

   -- Create proc threads
   threads <- forM (processors pf) $ \proc -> do
      trs <- atomically newTChan
      _ <- forkOS (procThread proc trs)
      return (proc, trs)

   ks <- atomically $ TMap.empty
      
   return $ KernelManager rm (fromList threads) ks


-- | Thread that perform kernel execution on a given proc
procThread :: Processor -> TChan (KernelExecution, KernelEvent) -> IO ()
procThread proc ch = do
   (exec,ev) <- atomically $ readTChan ch

   err <- execute proc (executableKernel exec) (kernelParameters exec)
   setEvent ev err

   procThread proc ch


-- | Register a kernel and load associated information saved from previous runs
registerKernel :: KernelManager -> Kernel -> IO ()
registerKernel km k = do

   -- Load compiled kernels
   ck <- atomically $ TMap.empty

   let info = KernelInfo ck

   atomically $ TMap.insert k info (registered km)


-- | Compile a kernel for the given processors and return those for which it succeeded
compileKernel :: KernelManager -> Kernel -> [Processor] -> IO [Processor]
compileKernel km k ps = do
   cks <- compile k ps
   let valids = catMaybes $ Prelude.map (\(x,y) -> fmap (x,) y) $ ps `zip` cks

   atomically $ do
      info <- (registered km) TMap.! k
      forM_ valids $ \(proc,ck) -> 
         TMap.insert proc ck (compiled info)

   return $ Prelude.map fst valids


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
executeKernel :: KernelManager -> Processor -> KernelExecution -> IO ()
executeKernel km proc exec = do
   
   let ch = procWorkers km Map.! proc

   ev <- newEvent
   atomically $ writeTChan ch (exec, ev)
   waitEvent ev
   atomically $ cancelKernelExecution km exec
