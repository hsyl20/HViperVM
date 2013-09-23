module ViperVM.VirtualPlatform.MetaKernel (
   MetaKernel(..), Arg(..), AccessMode(..)
) where

import ViperVM.Platform.Kernel
import ViperVM.Platform.KernelParameter
import ViperVM.VirtualPlatform.Object

-- | A meta kernel
-- Set of kernels performing the same operation
data MetaKernel = MetaKernel {
   name :: String,
   proto :: [Arg],
   paramsFromObjects :: [ObjectPeer] -> [KernelParameter],
   kernels :: [Kernel]
}

-- | Parameter access modes
data AccessMode = ReadOnly | WriteOnly | ReadWrite
   deriving (Eq,Ord)

-- | Kernel parameter prototype
data Arg = Arg {
   mode :: AccessMode,
   label :: String
}

