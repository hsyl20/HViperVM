{-# LANGUAGE LambdaCase #-}
module ViperVM.Library.FloatMatrixMul (
  floatMatrixMulKernelCL, floatMatrixMulObjectKernelCL,
  floatMatrixMulBuiltin
  ) where

import ViperVM.Platform
import ViperVM.Platform.Descriptor
import qualified ViperVM.Platform.Primitive as Prim
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Runtime (MakeBuiltin)
import ViperVM.Platform.KernelParameter
import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Backends.OpenCL.Kernel (clUIntParam,clMemParam)
import ViperVM.Graph.Builtins
import Control.Applicative ( (<$>) )
import Paths_ViperVM

floatMatrixMulKernelCL :: IO CL.Kernel
floatMatrixMulKernelCL = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixMul.cl"
   CL.initKernelFromFile fileName "floatMatrixMul" [] "" configFromParamsCL

configFromParamsCL :: [KernelParameter] -> CL.KernelConfiguration
configFromParamsCL pms = CL.KernelConfiguration gDim lDim clParams
   where
      [WordParam k,
       BufferParam (CLBuffer a), 
       WordParam lda,
       WordParam offa,
       BufferParam (CLBuffer b), 
       WordParam ldb,
       WordParam offb,
       BufferParam (CLBuffer c), 
       WordParam ldc,
       WordParam offc,
       WordParam w,
       WordParam h] = pms

      roundTo to v = v + (if ms /= 0 then to - ms else 0)
         where ms = mod v to

      gDim = [roundTo 32 w, roundTo 32 h,1]

      lDim = [32,32,1]

      clParams = [clUIntParam k, 
                  clUIntParam w,
                  clUIntParam h,
                  clMemParam a,
                  clUIntParam lda,
                  clUIntParam offa,
                  clMemParam b,
                  clUIntParam ldb,
                  clUIntParam offb,
                  clMemParam c,
                  clUIntParam ldc,
                  clUIntParam offc]


floatMatrixMulObjectKernelCL :: IO ObjectKernel
floatMatrixMulObjectKernelCL = do
   let modes = [ReadOnly,ReadOnly,ReadWrite]
   ker <- CLKernel <$> floatMatrixMulKernelCL
   return (ObjectKernel ker modes paramsFromObjects)



paramsFromObjects :: [Object] -> KernelObjectConfig
paramsFromObjects objs = KernelObjectConfig pms roRegions rwRegions
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs

      pms = [WordParam (fromIntegral k),
             BufferParam (matrixBuffer ma),
             WordParam (fromIntegral lda), 
             WordParam (fromIntegral $ matrixOffset ma),
             BufferParam (matrixBuffer mb),
             WordParam (fromIntegral ldb), 
             WordParam (fromIntegral $ matrixOffset mb),
             BufferParam (matrixBuffer mc),
             WordParam (fromIntegral ldc), 
             WordParam (fromIntegral $ matrixOffset mc),
             WordParam (fromIntegral w),
             WordParam (fromIntegral h)]

      (k, _) = matrixDimensions ma
      w = matrixWidth mb
      h = matrixHeight ma
      lda = ((matrixWidth ma) * 4 + (matrixPadding ma)) `div` 4
      ldb = ((matrixWidth mb) * 4 + (matrixPadding mb)) `div` 4
      ldc = ((matrixWidth mc) * 4 + (matrixPadding mc)) `div` 4
      a = matrixBuffer ma
      b = matrixBuffer mb
      c = matrixBuffer mc
      roRegions = [(a, matrixRegion ma), (b, matrixRegion mb)]
      rwRegions = [(c, matrixRegion mc)]


floatMatrixMulBuiltin :: MakeBuiltin
floatMatrixMulBuiltin readData writeData exec alloc = do

   ok <- floatMatrixMulObjectKernelCL

   return $ Builtin [True,True] $ \case
      ([x',y'],_) -> do
         let 
            (x,y) = (readData x', readData y')
            (w,_) = matrixDescDims (descriptor y)
            (_,h) = matrixDescDims (descriptor x)
            desc  = MatrixDesc Prim.Float w h
         e <- alloc desc
         exec ok [x,y,e]
         return (writeData e)
      _ -> error "Bad kernel arguments"
