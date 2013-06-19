{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixTranspose (
  floatMatrixTransposeKernelCL, floatMatrixTransposeObjectKernelCL,
  floatMatrixTransposeBuiltin
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

floatMatrixTransposeKernelCL :: IO CL.Kernel
floatMatrixTransposeKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixTranspose.cl"
   CL.initKernelFromFile fileName "floatMatrixTranspose" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [WordParam width, 
       WordParam height, 
       BufferParam (CLBuffer a), 
       WordParam lda,
       WordParam offa,
       BufferParam (CLBuffer b), 
       WordParam ldb,
       WordParam offb] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [roundTo 16 width, roundTo 16 height,1]

      lDim = [16,16,1]

      clParams = [clUIntParam width, 
                  clUIntParam height, 
                  clMemParam a,
                  clUIntParam lda,
                  clUIntParam offa,
                  clMemParam b,
                  clUIntParam ldb,
                  clUIntParam offb]


floatMatrixTransposeObjectKernelCL :: IO ObjectKernel
floatMatrixTransposeObjectKernelCL = do
   let modes = [ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixTransposeKernelCL
   return (ObjectKernel ker modes paramsFromObjects)

paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb] = objs

      pms = [WordParam (fromIntegral width), 
             WordParam (fromIntegral height), 
             BufferParam (matrixBuffer ma), 
             WordParam (fromIntegral lda), 
             WordParam (fromIntegral $ matrixOffset ma),
             BufferParam (matrixBuffer mb),
             WordParam (fromIntegral ldb), 
             WordParam (fromIntegral $ matrixOffset mb)]

      (width, height) = matrixDimensions ma
      lda = ((matrixWidth ma) * 4 + (matrixPadding ma)) `div` 4
      ldb = ((matrixWidth mb) * 4 + (matrixPadding mb)) `div` 4
      a = matrixBuffer ma
      b = matrixBuffer mb
      roRegions = [(a, matrixRegion ma)]
      rwRegions = [(b, matrixRegion mb)]


floatMatrixTransposeBuiltin :: MakeBuiltin
floatMatrixTransposeBuiltin readData writeData exec alloc = do

   ok <- floatMatrixTransposeObjectKernelCL

   return $ Builtin [True] $ \case
      ([x'],_) -> do
         let x = readData x'
         e <- alloc (descriptor x)
         exec ok [x,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
