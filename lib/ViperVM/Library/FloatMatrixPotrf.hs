{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixPotrf (
  floatMatrixPotrfKernelCL, floatMatrixPotrfObjectKernelCL,
  floatMatrixPotrfBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Runtime (MakeBuiltin)
import ViperVM.Platform.KernelParameter
import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Backends.OpenCL.Kernel (clUIntParam,clMemParam)
import ViperVM.Graph.Builtins
import Control.Applicative ( (<$>) )
import Paths_ViperVM

floatMatrixPotrfKernelCL :: IO CL.Kernel
floatMatrixPotrfKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixPotrf.cl"
   CL.initKernelFromFile fileName "floatMatrixPotrf" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [WordParam n, 
       WordParam srcOffset, 
       WordParam srcWidth, 
       BufferParam (CLBuffer srcBuf),
       WordParam dstOffset, 
       WordParam dstWidth, 
       BufferParam (CLBuffer dstBuf)] = pms
      gDim = [32,32,1]
      lDim = [32,32,1]
      clParams = [clUIntParam n, 
                  clUIntParam srcOffset, 
                  clUIntParam srcWidth, 
                  clMemParam srcBuf,
                  clUIntParam dstOffset, 
                  clUIntParam dstWidth, 
                  clMemParam dstBuf]


floatMatrixPotrfObjectKernelCL :: IO ObjectKernel
floatMatrixPotrfObjectKernelCL = do
   let modes = [ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixPotrfKernelCL
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
      pms = [ -- FIXME: offsets and paddings must be divisible by 4
            WordParam (fromIntegral width), 
            WordParam (fromIntegral (srcOffset `div` 4)), 
            WordParam (fromIntegral ((srcSize + srcPadding) `div` 4)), 
            BufferParam srcBuf, 
            WordParam (fromIntegral (dstOffset `div` 4)), 
            WordParam (fromIntegral ((dstSize + dstPadding) `div` 4)), 
            BufferParam dstBuf
         ]
      roRegions = [(srcBuf, matrixRegion msrc)]
      rwRegions = [(dstBuf, matrixRegion mdst)]

floatMatrixPotrfBuiltin :: MakeBuiltin
floatMatrixPotrfBuiltin readData writeData exec alloc = do

   ok <- floatMatrixPotrfObjectKernelCL

   return $ Builtin [True] $ \case
      ([x'],_) -> do
         let x = readData x'
         e <- alloc (descriptor x)
         exec ok [x,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
