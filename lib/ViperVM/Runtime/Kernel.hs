module ViperVM.Runtime.Kernel (
   registerKernel, registerKernelIO, registerKernelsIO,
   fetchMetaKernel, storeCompiledKernel,
   beforeKernelCompilation, afterKernelCompilation
) where

import ViperVM.Runtime.Nodes

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import qualified ViperVM.Platform as Pf
import qualified ViperVM.STM.TMap as TMap
import qualified ViperVM.STM.TSet as TSet

-- Find or create MetaKernel node
fetchMetaKernel :: Runtime -> KernelInterface -> STM MetaKernel
fetchMetaKernel r ki = do
   kernelMap <- readTVar (kernels r)
   case Map.lookup ki kernelMap of
      Nothing -> do
         mk <- MetaKernel ki <$> TSet.empty
         writeTVar (kernels r) (Map.insert ki mk kernelMap)
         return mk
      Just mk -> return mk

-- | Register several kernels at once
registerKernelsIO :: Runtime -> KernelInterface -> [Pf.Kernel] -> IO ()
registerKernelsIO r ki ks = forM_ ks (registerKernelIO r ki)

-- | IO version of registerKernel
registerKernelIO :: Runtime -> KernelInterface -> Pf.Kernel -> IO ()
registerKernelIO r ki k = atomically $ registerKernel r ki k

-- | Register a kernel in the runtime system
registerKernel :: Runtime -> KernelInterface -> Pf.Kernel -> STM ()
registerKernel r ki k = do
   meta <- fetchMetaKernel r ki
   ker <- Kernel k <$> newTVar Map.empty <*> TSet.empty
   TSet.insert ker (metaKernels meta)
   writeTChan (events r) $ NotifyKernelRegister ki ker


-- | Store a compiled kernel for several processors
storeCompiledKernel :: Kernel -> [Processor] -> Pf.CompiledKernel -> STM ()
storeCompiledKernel k ps ck = forM_ ps $ \p -> TMap.insert p ck (kernelCompiled k)


-- | Indicate in the graph that a compilation has started
beforeKernelCompilation :: Kernel -> [Processor] -> STM ()
beforeKernelCompilation k ps = forM_ ps $ \p -> TSet.insert p (kernelBeingCompiled k)


-- | Remove indication of a kernel being compiled
afterKernelCompilation :: Kernel -> [Processor] -> STM ()
afterKernelCompilation k ps = forM_ ps $ \p -> TSet.delete p (kernelBeingCompiled k)
