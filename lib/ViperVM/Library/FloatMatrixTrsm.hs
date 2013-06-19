{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixTrsm (
  floatMatrixTrsmKernelCL, floatMatrixTrsmObjectKernelCL,
  floatMatrixTrsmBuiltin
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

floatMatrixTrsmKernelCL :: IO CL.Kernel
floatMatrixTrsmKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixTrsm.cl"
   CL.initKernelFromFile fileName "floatMatrixTrsm" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [BufferParam (CLBuffer a), 
       WordParam lda,
       WordParam offa,
       BufferParam (CLBuffer b), 
       WordParam ldb,
       WordParam offb,
       BufferParam (CLBuffer c), 
       WordParam ldc,
       WordParam offc,
       WordParam n,
       WordParam valid] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [1, roundTo 128 valid,1]

      lDim = [1,128,1]

      clParams = [clMemParam a,
                  clUIntParam lda,
                  clUIntParam offa,
                  clMemParam b,
                  clUIntParam ldb,
                  clUIntParam offb,
                  clMemParam c,
                  clUIntParam ldc,
                  clUIntParam offc,
                  clUIntParam n,
                  clUIntParam valid]


floatMatrixTrsmObjectKernelCL :: IO ObjectKernel
floatMatrixTrsmObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixTrsmKernelCL
   return (ObjectKernel ker modes paramsFromObjects)



paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs

      pms = [BufferParam (matrixBuffer ma),
             WordParam (fromIntegral lda), 
             WordParam (fromIntegral $ matrixOffset ma),
             BufferParam (matrixBuffer mb),
             WordParam (fromIntegral ldb), 
             WordParam (fromIntegral $ matrixOffset mb),
             BufferParam (matrixBuffer mc),
             WordParam (fromIntegral ldc), 
             WordParam (fromIntegral $ matrixOffset mc),
             WordParam (fromIntegral n),
             WordParam (fromIntegral valid)]

      n = matrixWidth mb
      valid = matrixHeight mc
      lda = ((matrixWidth ma) * 4 + (matrixPadding ma)) `div` 4
      ldb = ((matrixWidth mb) * 4 + (matrixPadding mb)) `div` 4
      ldc = ((matrixWidth mc) * 4 + (matrixPadding mc)) `div` 4
      a = matrixBuffer ma
      b = matrixBuffer mb
      c = matrixBuffer mc
      roRegions = [(a, matrixRegion ma), (b, matrixRegion mb)]
      rwRegions = [(c, matrixRegion mc)]


floatMatrixTrsmBuiltin :: MakeBuiltin
floatMatrixTrsmBuiltin readData writeData exec alloc = do

   ok <- floatMatrixTrsmObjectKernelCL

   return $ Builtin [True,True] $ \case
      ([x',y'],_) -> do
         let 
            (x,y) = (readData x', readData y')
         e <- alloc (descriptor x)
         exec ok [x,y,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
