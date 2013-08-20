module ViperVM.Library.FloatMatrixAdd.MetaKernel (
   metaKernel
) where

import Control.Applicative ((<$>))
import ViperVM.VirtualPlatform.MetaKernel hiding (proto,name,kernels)
import ViperVM.VirtualPlatform.Objects.Matrix
import ViperVM.VirtualPlatform.Object
import ViperVM.Platform.KernelParameter

import ViperVM.Library.FloatMatrixAdd.Kernels


metaKernel :: IO MetaKernel
metaKernel = MetaKernel name proto conf <$> kernels
   where
      name = "FloatMatrixAdd"
      proto = [
            Arg ReadOnly "a",
            Arg ReadOnly "b",
            Arg WriteOnly "c"
         ]

conf :: [ObjectPeer] -> [KernelParameter]
conf objs = params
   where
      [MatrixObject ma, MatrixObject mb, MatrixObject mc] = objs
      params = 
         [WordParam (fromIntegral width), 
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
