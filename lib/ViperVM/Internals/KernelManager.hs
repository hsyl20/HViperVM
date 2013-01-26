module ViperVM.Internals.KernelManager (
   KernelManager,
   ViperVM.Internals.KernelManager.init,
   register,
   associate,
   compiledFor
) where

import Data.Map
import qualified Data.Map as Map

import ViperVM.Kernel
import ViperVM.Platform

data KernelManager = KernelManager {
      kernels :: Map Kernel (Map Processor CompiledKernel)
   }

-- | Initialize kernel manager
init :: KernelManager
init = KernelManager Map.empty

-- | Register a kernel
register :: KernelManager -> Kernel -> KernelManager
register manager k = KernelManager $ Map.insert k Map.empty (kernels manager)

-- | Associate a compiled kernel
associate :: KernelManager -> Kernel -> Processor -> CompiledKernel -> KernelManager
associate manager k p ck = KernelManager $ Map.insertWith Map.union k (Map.singleton p ck) (kernels manager)

-- | Retrieve compiled kernel for a given processor if any
compiledFor :: KernelManager -> Processor -> Kernel -> Maybe CompiledKernel
compiledFor manager proc k = Map.lookup proc =<< Map.lookup k (kernels manager)
