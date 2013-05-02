{-# LANGUAGE TupleSections #-}

module ViperVM.Platform.KernelManager (
   KernelManager, KernelEvent, 
   createKernelManager, registerKernel, compileKernel,
   prepareKernelExecution
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Processor
import ViperVM.Platform.Kernel
import ViperVM.Platform.RegionManager
import ViperVM.STM.TMap as TMap

import Data.Map
import Data.Maybe
import Data.Foldable (forM_)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forM)

data PrepareExecutionResult = PrepareExecutionSuccess | RegionAlreadyLocked

type KernelEvent = TVar (Maybe ())

data KernelManager = KernelManager {
      regionManager :: RegionManager,
      procWorkers :: Map Processor (TChan (CompiledKernel, KernelConfiguration, KernelEvent)),
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
      trs <- atomically $ newTChan
      _ <- forkOS (procThread proc trs)
      return (proc, trs)

   ks <- atomically $ TMap.empty
      
   return $ KernelManager rm (fromList threads) ks


-- | Thread that perform kernel execution on a given proc
procThread :: Processor -> TChan (CompiledKernel, KernelConfiguration, KernelEvent) -> IO ()
procThread proc trs = do
   (k,conf,ev) <- atomically $ readTChan trs

   err <- execute proc k conf
   atomically $ writeTVar ev (Just err)

   procThread proc trs


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
prepareKernelExecution :: KernelManager -> CompiledKernel -> KernelConfiguration -> STM PrepareExecutionResult
prepareKernelExecution km ck conf = do
  return PrepareExecutionSuccess
