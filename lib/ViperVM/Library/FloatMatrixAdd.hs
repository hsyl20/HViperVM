module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAddCL
  ) where

import ViperVM.Platform

floatMatrixAddCL :: Kernel
floatMatrixAddCL = CLKernel {
   kernelName = "floatMatrixAdd",
   constraints = [],
   options = "",
   configure = floatMatrixAddCLConfig,
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
}

floatMatrixAddCLConfig :: [KernelParameter] -> CLKernelConfiguration
floatMatrixAddCLConfig params = CLKernelConfiguration gDim lDim clParams
   where
      [IntParam width, IntParam height, BufferParam a, BufferParam b, BufferParam c] = params
      gDim = [width + (mod width 32), height + (mod height 32),1]
      lDim = [32,32,1]
      clParams = [clIntParam width, clIntParam height, 
                  clMemParam a, clMemParam b, clMemParam c]


--floatMatrixAdd :: KernelInterface
--floatMatrixAdd = KernelInterface {
--  name = "Float Matrix Addition",
--  paramCount = (2,1),
--  makeParameters = \ [a,b] -> [KPReadOnly a, KPReadOnly b, KPReadOnly a],
--  makeResult = \[_,_,c] -> [c]
--}

