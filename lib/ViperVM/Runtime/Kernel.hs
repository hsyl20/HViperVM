module ViperVM.Runtime.Kernel (
   registerKernel, registerKernelIO, registerKernelsIO,
   fetchMetaKernel
) where

import ViperVM.Runtime.Nodes

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import Control.Applicative
import qualified ViperVM.STM.TSet as TSet
import qualified ViperVM.Platform as Pf

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
   ker <- Kernel k <$> newTVar Map.empty
   TSet.insert ker (metaKernels meta)
