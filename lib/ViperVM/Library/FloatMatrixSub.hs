module ViperVM.Library.FloatMatrixSub (
   builtin, function, metaKernel, kernels
) where

import Control.Applicative ( (<$>) )

import ViperVM.VirtualPlatform.FunctionalKernel hiding (metaKernel,proto)
import ViperVM.VirtualPlatform.MetaObject
import ViperVM.VirtualPlatform.Descriptor
import ViperVM.VirtualPlatform.MetaKernel hiding (proto,name,kernels)
import ViperVM.VirtualPlatform.Objects.Matrix
import ViperVM.VirtualPlatform.Object

import ViperVM.Platform.KernelParameter
import ViperVM.Platform.Kernel
import ViperVM.Platform.Peer.KernelPeer

import qualified ViperVM.Library.OpenCL.FloatMatrixSub as CL

----------------------------------------
-- Builtin & Function
---------------------------------------

builtin :: MakeBuiltin
builtin = makeBuiltinIO function

function :: IO FunctionalKernel
function = FunctionalKernel proto makeParams makeResult <$> metaKernel
   where
      proto = Prototype {
            inputs = [MatrixType,MatrixType],
            output = MatrixType
         }

      makeParams args = do
         let [a,b] = args
         c <- allocate (descriptor a)
         return [a,b,c]

      makeResult args = last args


----------------------------------------
-- MetaKernel
---------------------------------------

metaKernel :: IO MetaKernel
metaKernel = MetaKernel name proto conf <$> kernels
   where
      name = "FloatMatrixSub"
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


----------------------------------------
-- Kernels
---------------------------------------

kernels :: IO [Kernel]
kernels = initKernelsIO [
      CLKernel <$> CL.kernel
   ]
