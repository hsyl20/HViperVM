{-# LANGUAGE LambdaCase #-}

-- | Backend specific glue for kernels
module ViperVM.Platform.Peer.KernelPeer (
   KernelPeer(..), kernelConstraints, kernelExecute
) where

import qualified ViperVM.Backends.OpenCL.Kernel as CL
import qualified ViperVM.Backends.OpenCL.Buffer as CL
import ViperVM.Platform.Memory
import ViperVM.Platform.Peer.MemoryPeer
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.Peer.ProcPeer

data KernelPeer = CLKernel CL.Kernel
                  deriving (Show,Ord,Eq)


-- | Retrieve kernel constraints
kernelConstraints :: KernelPeer -> [KernelConstraint]
kernelConstraints (CLKernel k) = CL.kernelConstraints k

-- | Execute a kernel 
-- Convert kernel parameters to the backend format before execution
kernelExecute :: KernelPeer -> [KernelParameter] -> ProcPeer -> IO ()
kernelExecute (CLKernel k) params (CLProc proc) =
   CL.kernelExecute proc k (fmap convertCLParam params)

-- | Convert kernel parameters to OpenCL kernel parameters
convertCLParam :: KernelParameter -> CL.CLKernelParameter
convertCLParam = \case 
   IntParam i  -> CL.CLIntParam (fromIntegral i)
   WordParam i -> CL.CLUIntParam (fromIntegral i)
   BufferParam b -> CL.CLMemParam . CL.bufferPeer . clBuffer $ bufferPeer b
   _ -> error "Kernel parameter type not supported yet"
