{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAddKernelCL, floatMatrixAddObjectKernelCL,
  floatMatrixAddBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Runtime (MakeBuiltin)
import ViperVM.Graph.Builtins
import Paths_ViperVM

floatMatrixAddKernelCL :: IO Kernel
floatMatrixAddKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixAdd.cl"
   initCLKernelFromFile fileName "floatMatrixAdd" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CLKernelConfiguration
configFromParamsCL pms = CLKernelConfiguration gDim lDim clParams
   where
      [WordParam width, WordParam height, BufferParam a, BufferParam b, BufferParam c] = pms
      gDim = [width + (mod width 32), height + (mod height 32),1]
      lDim = [32,32,1]
      clParams = [clUIntParam width, clUIntParam height, 
                  clMemParam a, clMemParam b, clMemParam c]


floatMatrixAddObjectKernelCL :: IO ObjectKernel
floatMatrixAddObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- floatMatrixAddKernelCL
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


floatMatrixAddBuiltin :: MakeBuiltin
floatMatrixAddBuiltin readData writeData exec alloc = do

   ok <- floatMatrixAddObjectKernelCL

   return $ Builtin [True,True] $ \case
      ([x',y'],_) -> do
         let (x,y) = (readData x', readData y')
         e <- alloc (descriptor x)
         exec ok [x,y,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
