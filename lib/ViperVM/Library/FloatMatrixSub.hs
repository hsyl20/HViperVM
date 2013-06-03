module ViperVM.Library.FloatMatrixSub (
  floatMatrixSubKernelCL, floatMatrixSubObjectKernelCL
  ) where

import ViperVM.Platform
import Paths_ViperVM

floatMatrixSubKernelCL :: IO Kernel
floatMatrixSubKernelCL = do
   src <- readFile =<< getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixSub.cl"
   return $ CLKernel {
         kernelName = "floatMatrixSub",
         constraints = [],
         options = "",
         configure = configFromParamsCL,
         source = src
      }

configFromParamsCL :: [KernelParameter] -> CLKernelConfiguration
configFromParamsCL pms = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam width, WordParam height, BufferParam a, BufferParam b, BufferParam c] = pms
      gDim = [width + (mod width 32), height + (mod height 32),1]
      lDim = [32,32,1]
      clParams = [clUIntParam width, clUIntParam height, 
                  clMemParam a, clMemParam b, clMemParam c]


floatMatrixSubObjectKernelCL :: IO ObjectKernel
floatMatrixSubObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- floatMatrixSubKernelCL
   return (ObjectKernel ker modes paramsFromObjects)

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
