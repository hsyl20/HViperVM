{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixSub (
  floatMatrixSubKernelCL, floatMatrixSubObjectKernelCL,
  makeFloatMatrixSubBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins
import ViperVM.STM.TMap as TMap

import Control.Concurrent.STM

import Paths_ViperVM

floatMatrixSubKernelCL :: IO Kernel
floatMatrixSubKernelCL = do
   src <- readFile =<< getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixSub.cl"
   compiled <- atomically TMap.empty
   return $ CLKernel {
         kernelName = "floatMatrixSub",
         constraints = [],
         options = "",
         configure = configFromParamsCL,
         source = src,
         compilations = compiled
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


makeFloatMatrixSubBuiltin :: (Expr -> SharedObject) -> (SharedObject -> Expr) -> (ObjectKernel -> [SharedObject] -> IO ()) -> (Descriptor -> IO SharedObject) -> IO Builtin
makeFloatMatrixSubBuiltin readData writeData exec alloc = do

   ok <- floatMatrixSubObjectKernelCL

   return $ Builtin [True,True] $ \case
      ([x',y'],_) -> do
         let (x,y) = (readData x', readData y')
         e <- alloc (descriptor x)
         exec ok [x,y,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
