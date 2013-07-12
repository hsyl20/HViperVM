module ViperVM.Platform.Peer.KernelPeer (
   KernelPeer(..), kernelConstraints
) where

import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Platform.KernelConstraint

data KernelPeer = CLKernel CL.Kernel
                  deriving (Show)


kernelConstraints :: KernelPeer -> [KernelConstraint]
kernelConstraints (CLKernel k) = CL.kernelConstraints k
