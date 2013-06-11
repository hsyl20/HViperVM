module ViperVM.Runtime.Kernel (
   fetchMetaKernel,
   beforeKernelCompilation, afterKernelCompilation
) where

import ViperVM.Runtime.Nodes

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
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

-- | Indicate in the graph that a compilation has started
beforeKernelCompilation :: Kernel -> [Processor] -> STM ()
beforeKernelCompilation k ps = forM_ ps $ \p -> TSet.insert p (kernelBeingCompiled k)


-- | Remove indication of a kernel being compiled
afterKernelCompilation :: Kernel -> [Processor] -> STM ()
afterKernelCompilation k ps = forM_ ps $ \p -> TSet.delete p (kernelBeingCompiled k)
