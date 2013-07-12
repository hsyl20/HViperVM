{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixAdd (
  floatMatrixAddKernelCL, floatMatrixAddObjectKernelCL,
  floatMatrixAddBuiltin
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

floatMatrixAddKernelCL :: IO CL.Kernel
floatMatrixAddKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixAdd.cl"
   CL.initKernelFromFile fileName "floatMatrixAdd" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [WordParam width, 
       WordParam height, 
       BufferParam a,
       WordParam lda,
       WordParam offa,
       BufferParam b,
       WordParam ldb,
       WordParam offb,
       BufferParam c,
       WordParam ldc,
       WordParam offc] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [roundTo 16 width, roundTo 16 height,1]

      lDim = [16,16,1]

      clParams = [clUIntParam width, 
                  clUIntParam height, 
                  clMemParam (clBuffer a),
                  clUIntParam lda,
                  clUIntParam offa,
                  clMemParam (clBuffer b),
                  clUIntParam ldb,
                  clUIntParam offb,
                  clMemParam (clBuffer c),
                  clUIntParam ldc,
                  clUIntParam offc]


floatMatrixAddObjectKernelCL :: IO ObjectKernel
floatMatrixAddObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixAddKernelCL
   return (ObjectKernel ker modes paramsFromObjects)

paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs
      pms = [WordParam (fromIntegral width), 
             WordParam (fromIntegral height), 
             BufferParam (matrixBuffer ma),
             WordParam (fromIntegral lda), 
             WordParam (fromIntegral (matrixOffset ma `div` 4)),
             BufferParam (matrixBuffer mb),
             WordParam (fromIntegral ldb), 
             WordParam (fromIntegral (matrixOffset mb `div` 4)),
             BufferParam (matrixBuffer mc),
             WordParam (fromIntegral ldc), 
             WordParam (fromIntegral (matrixOffset mc `div` 4))]

      (width, height) = matrixDimensions ma
      lda = ((matrixWidth ma) * 4 + (matrixPadding ma)) `div` 4
      ldb = ((matrixWidth mb) * 4 + (matrixPadding mb)) `div` 4
      ldc = ((matrixWidth mc) * 4 + (matrixPadding mc)) `div` 4
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
