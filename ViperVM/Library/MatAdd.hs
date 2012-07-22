module ViperVM.Library.MatAdd where

import ViperVM.Kernel
import ViperVM.KernelSet

matrixAddCL :: Kernel
matrixAddCL = CLKernel "matrixAdd" [] "" "\
  \__kernel void matrixAdd(const int width, const int height,\
  \                        __global float* A, __global float* B, __global float* C) {\
  \  int gx = get_global_id(0);\
  \  int gy = get_global_id(1);\
  \\
  \  if (gx < width && gy < height) {\
  \    C[gy*width+gx] = A[gy*width+gx] + B[gy*width+gx];\
  \  }\
  \\
  \}"

matAdd :: KernelSet
matAdd = KernelSet [matrixAddCL]
