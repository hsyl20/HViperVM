module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAdd,
  floatMatrixAddCL
  ) where

import ViperVM.Kernel
import ViperVM.KernelInterface
import ViperVM.KernelSet

floatMatrixAddCL :: Kernel
floatMatrixAddCL = CLKernel "floatMatrixAdd" [] "" "\
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


floatMatrixAddInterface :: KernelInterface
floatMatrixAddInterface = KernelInterface {
  name = "Float Matrix Addition",
  modes = [ReadOnly,ReadOnly,ReadWrite]
}

floatMatrixAdd :: KernelSet
floatMatrixAdd = KernelSet floatMatrixAddInterface [floatMatrixAddCL]
