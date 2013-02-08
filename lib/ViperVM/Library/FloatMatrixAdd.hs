module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAdd,
  floatMatrixAddCL
  ) where

import ViperVM.Runtime.Kernel
import ViperVM.KernelInterface
import ViperVM.KernelSet
import ViperVM.Runtime.Data
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region

floatMatrixAddCL :: Kernel
floatMatrixAddCL = CLKernel {
   kernelName = "floatMatrixAdd",
   constraints = [],
   options = "",
   source = "\
  \__kernel void floatMatrixAdd(const int width, const int height,\
  \                        __global float* A, __global float* B, __global float* C) {\
  \  int gx = get_global_id(0);\
  \  int gy = get_global_id(1);\
  \\
  \  if (gx < width && gy < height) {\
  \    C[gy*width+gx] = A[gy*width+gx] + B[gy*width+gx];\
  \  }\
  \\
  \}"
  --configure = floatMatrixAddCLConfig
}

floatMatrixAddCLConfig :: [(DataDesc,DataInstance)] -> KernelConfiguration
floatMatrixAddCLConfig dis = CLKernelConfiguration gDim lDim params
   where
      -- TODO: we shouldn't use vectors. We should use region offset
      (VectorDesc PrimFloat n1, Vector buf1 (Region1D _ _)) = head dis
      (VectorDesc PrimFloat _, Vector buf2 (Region1D _ _)) = head (tail dis)
      (VectorDesc PrimFloat _, Vector buf3 (Region1D _ _)) = head (tail (tail dis))
      gDim = [fromIntegral n1,1,1]
      lDim = []
      params = [CLKPInt (fromIntegral n1), CLKPInt 1, CLKPMem (getCLBuffer buf1), CLKPMem (getCLBuffer buf2), CLKPMem (getCLBuffer buf3)]


floatMatrixAddInterface :: KernelInterface
floatMatrixAddInterface = KernelInterface {
  name = "Float Matrix Addition",
  paramCount = (2,1),
  makeParameters = \ [a,b] -> [KPReadOnly a, KPReadOnly b, KPReadOnly a],
  makeResult = \[_,_,c] -> [c]
}

floatMatrixAdd :: KernelSet
floatMatrixAdd = KernelSet floatMatrixAddInterface [floatMatrixAddCL]
