{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixSub (
  floatMatrixSubKernelCL, floatMatrixSubObjectKernelCL,
  floatMatrixSubBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Objects.Matrix
import ViperVM.Platform.Runtime (MakeBuiltin)
import ViperVM.Platform.KernelParameter
import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Backends.OpenCL.Kernel (clUIntParam,clMemParam)
import ViperVM.Graph.Builtins
import Control.Applicative ( (<$>) )
import Paths_ViperVM

floatMatrixSubKernelCL :: IO CL.Kernel
floatMatrixSubKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixSub.cl"
   CL.initKernelFromFile fileName "floatMatrixSub" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [WordParam width, 
       WordParam height, 
       BufferParam (CLBuffer a), 
       BufferParam (CLBuffer b), 
       BufferParam (CLBuffer c)] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [roundTo 16 width, roundTo 16 height,1]

      lDim = [16,16,1]

      clParams = [clUIntParam width, 
                  clUIntParam height, 
                  clMemParam a, 
                  clMemParam b, 
                  clMemParam c]


floatMatrixSubObjectKernelCL :: IO ObjectKernel
floatMatrixSubObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixSubKernelCL
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
