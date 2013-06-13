{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixSub (
  floatMatrixSubKernelCL, floatMatrixSubObjectKernelCL,
  floatMatrixSubBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Runtime (MakeBuiltin)
import ViperVM.Graph.Builtins
import Paths_ViperVM

floatMatrixSubKernelCL :: IO Kernel
floatMatrixSubKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixSub.cl"
   initCLKernelFromFile fileName "floatMatrixSub" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CLKernelConfiguration
configFromParamsCL pms = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam width, 
       WordParam height, 
       BufferParam a, 
       BufferParam b, 
       BufferParam c] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [roundTo 32 width, roundTo 32 height,1]

      lDim = [32,32,1]

      clParams = [clUIntParam width, 
                  clUIntParam height, 
                  clMemParam a, 
                  clMemParam b, 
                  clMemParam c]


floatMatrixSubObjectKernelCL :: IO ObjectKernel
floatMatrixSubObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- floatMatrixSubKernelCL
   return (ObjectKernel ker modes paramsFromObjects)

paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs
      pms = [WordParam (fromIntegral width), 
             WordParam (fromIntegral height), 
             BufferParam a, 
             BufferParam b, 
             BufferParam c]
      (width, height) = matrixDimensions ma
      a = matrixBuffer ma
      b = matrixBuffer mb
      c = matrixBuffer mc
      roRegions = [(a, matrixRegion ma), (b, matrixRegion mb)]
      rwRegions = [(c, matrixRegion mc)]


floatMatrixSubBuiltin :: MakeBuiltin
floatMatrixSubBuiltin readData writeData exec alloc = do

   ok <- floatMatrixSubObjectKernelCL

   return $ Builtin [True,True] $ \case
      ([x',y'],_) -> do
         let (x,y) = (readData x', readData y')
         e <- alloc (descriptor x)
         exec ok [x,y,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
