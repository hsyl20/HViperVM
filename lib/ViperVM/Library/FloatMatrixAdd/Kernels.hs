module ViperVM.Library.FloatMatrixAdd.Kernels where

import Control.Applicative ( (<$>) )

import ViperVM.Platform.Kernel
import ViperVM.Platform.Peer.KernelPeer
import qualified ViperVM.Library.FloatMatrixAdd.OpenCL as CL

kernels :: IO [Kernel]
kernels = initKernelsIO [
      CLKernel <$> CL.kernel
   ]
