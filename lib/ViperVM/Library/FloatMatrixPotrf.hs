module ViperVM.Library.FloatMatrixPotrf (
  floatMatrixPotrfKernelCL, floatMatrixPotrfObjectKernelCL
  ) where

import ViperVM.Platform
import Paths_ViperVM

floatMatrixPotrfKernelCL :: IO Kernel
floatMatrixPotrfKernelCL = do
   src <- readFile =<< getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixPotrf.cl"
   return $ CLKernel {
         kernelName = "floatMatrixPotrf",
         constraints = [],
         options = "",
         configure = configFromParamsCL,
         source = src
      }

configFromParamsCL :: [KernelParameter] -> CLKernelConfiguration
configFromParamsCL pms = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam n, WordParam srcOffset, WordParam srcWidth, BufferParam srcBuf,
                    WordParam dstOffset, WordParam dstWidth, BufferParam dstBuf] = pms
      gDim = [32,32,1]
      lDim = [32,32,1]
      clParams = [clUIntParam n, clUIntParam srcOffset, clUIntParam srcWidth, clMemParam srcBuf,
                                 clUIntParam dstOffset, clUIntParam dstWidth, clMemParam dstBuf]


floatMatrixPotrfObjectKernelCL :: IO ObjectKernel
floatMatrixPotrfObjectKernelCL = do
   let modes = [ReadOnly,ReadWrite]
   ker <- floatMatrixPotrfKernelCL
   return (ObjectKernel ker modes paramsFromObjects)

paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject msrc, MatrixObject mdst] = objs
      srcBuf = matrixBuffer msrc
      dstBuf = matrixBuffer mdst
      (Region2D srcOffset _ srcSize srcPadding) = matrixRegion msrc
      (Region2D dstOffset _ dstSize dstPadding) = matrixRegion mdst
      width = matrixWidth msrc
      pms = [
            WordParam (fromIntegral width), 
            WordParam (fromIntegral srcOffset), 
            WordParam (fromIntegral (srcSize + srcPadding)), 
            BufferParam srcBuf, 
            WordParam (fromIntegral dstOffset), 
            WordParam (fromIntegral (dstSize + dstPadding)), 
            BufferParam dstBuf
         ]
      roRegions = [(srcBuf, matrixRegion msrc)]
      rwRegions = [(dstBuf, matrixRegion mdst)]
