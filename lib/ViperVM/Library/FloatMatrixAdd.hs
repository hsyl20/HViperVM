module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAddCL, paramsFromObjects
  ) where

import ViperVM.Platform

floatMatrixAddCL :: Kernel
floatMatrixAddCL = CLKernel {
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
configFromParamsCL params = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam width, WordParam height, BufferParam a, BufferParam b, BufferParam c] = params
      gDim = [width + (mod width 32), height + (mod height 32),1]
      lDim = [32,32,1]
      clParams = [clUIntParam width, clUIntParam height, 
                  clMemParam a, clMemParam b, clMemParam c]


paramsFromObjects :: [Object] -> ([KernelParameter], [(Buffer,Region)], [(Buffer,Region)])
paramsFromObjects objs = (params, readOnlyRegions, readWriteRegions)
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs
      params = [WordParam (fromIntegral width), WordParam (fromIntegral height), BufferParam a, BufferParam b, BufferParam c]
      (width, height) = matrixDimensions ma
      a = matrixBuffer ma
      b = matrixBuffer mb
      c = matrixBuffer mc
      readOnlyRegions = [(a, matrixRegion ma), (b, matrixRegion mb)]
      readWriteRegions = [(c, matrixRegion mc)]


--floatMatrixAdd :: KernelInterface
--floatMatrixAdd = KernelInterface {
--  name = "Float Matrix Addition",
--  paramCount = (2,1),
--  makeParameters = \ [a,b] -> [KPReadOnly a, KPReadOnly b, KPReadOnly a],
--  makeResult = \[_,_,c] -> [c]
--}

