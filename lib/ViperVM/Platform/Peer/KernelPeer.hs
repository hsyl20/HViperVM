module ViperVM.Platform.Peer.KernelPeer (
   KernelPeer(..), kernelConstraints, kernelExecute
) where

import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.KernelExecutionResult
import ViperVM.Platform.Peer.ProcPeer

data KernelPeer = CLKernel CL.Kernel
                  deriving (Show)


kernelConstraints :: KernelPeer -> [KernelConstraint]
kernelConstraints (CLKernel k) = CL.kernelConstraints k

kernelExecute :: KernelPeer -> [KernelParameter] -> ProcPeer -> IO ExecutionResult
kernelExecute (CLKernel k) params (CLProc proc) = CL.kernelExecute proc k params
