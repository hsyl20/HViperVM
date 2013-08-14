module ViperVM.VirtualPlatform.MetaKernel (
   MetaKernel(..), Arg(..), AccessMode(..)
) where

import ViperVM.Platform.Kernel

import Data.Set as Set

-- | A meta kernel
-- Set of kernels performing the same operation
data MetaKernel = MetaKernel {
   name :: String,
   kernels :: Set Kernel,
   proto :: [Arg]
}

-- | Parameter access modes
data AccessMode = ReadOnly | WriteOnly | ReadWrite

-- | Kernel parameter prototype
data Arg = Arg {
   mode :: AccessMode,
   label :: String
}

