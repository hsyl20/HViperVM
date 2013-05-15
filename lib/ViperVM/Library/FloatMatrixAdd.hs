module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAddKernelCL, floatMatrixAddObjectKernelCL
  ) where

import ViperVM.Platform

floatMatrixAddKernelCL :: Kernel
floatMatrixAddKernelCL = CLKernel {
   kernelName = "floatMatrixAdd",
   constraints = [],
   options = "",
   configure = configFromParamsCL,
   source = "\
  \__kernel void floatMatrixAdd(const uint width, const uint height,\
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

configFromParamsCL :: [KernelParameter] -> CLKernelConfiguration
configFromParamsCL pms = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam width, WordParam height, BufferParam a, BufferParam b, BufferParam c] = pms
      gDim = [width + (mod width 32), height + (mod height 32),1]
      lDim = [32,32,1]
      clParams = [clUIntParam width, clUIntParam height, 
                  clMemParam a, clMemParam b, clMemParam c]


floatMatrixAddObjectKernelCL :: ObjectKernel
floatMatrixAddObjectKernelCL = ObjectKernel floatMatrixAddKernelCL modes paramsFromObjects
   where
      modes = [ReadOnly,ReadOnly,ReadWrite]

paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs
      pms = [WordParam (fromIntegral width), WordParam (fromIntegral height), BufferParam a, BufferParam b, BufferParam c]
      (width, height) = matrixDimensions ma
      a = matrixBuffer ma
      b = matrixBuffer mb
      c = matrixBuffer mc
      roRegions = [(a, matrixRegion ma), (b, matrixRegion mb)]
      rwRegions = [(c, matrixRegion mc)]


--floatMatrixAdd :: KernelInterface
--floatMatrixAdd = KernelInterface {
--  name = "Float Matrix Addition",
--  paramCount = (2,1),
--  makeParameters = \ [a,b] -> [KPReadOnly a, KPReadOnly b, KPReadOnly a],
--  makeResult = \[_,_,c] -> [c]
--}

