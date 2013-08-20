module ViperVM.Library.FloatMatrixPotrf (
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

import qualified ViperVM.Library.OpenCL.FloatMatrixPotrf as CL

----------------------------------------
-- Builtin & Function
---------------------------------------

builtin :: MakeBuiltin
builtin = makeBuiltinIO function

function :: IO FunctionalKernel
function = FunctionalKernel proto makeParams makeResult <$> metaKernel
   where
      proto = Prototype {
            inputs = [MatrixType],
            output = MatrixType
         }

      makeParams args = do
         let [a] = args
         b <- allocate (descriptor a)
         return [a,b]

      makeResult args = last args


----------------------------------------
-- MetaKernel
---------------------------------------

metaKernel :: IO MetaKernel
metaKernel = MetaKernel name proto conf <$> kernels
   where
      name = "FloatMatrixPotrf"
      proto = [
            Arg ReadOnly "a",
            Arg ReadOnly "b"
         ]

      conf :: [ObjectPeer] -> [KernelParameter]
      conf objs = params
         where
            [MatrixObject ma, MatrixObject mb] = objs
            params = [ -- FIXME: offsets and paddings must be divisible by 4
                  WordParam (fromIntegral $ matrixWidth ma), 
                  WordParam (fromIntegral (matrixOffset ma `div` 4)), 
                  WordParam (fromIntegral lda), 
                  BufferParam (matrixBuffer ma), 
                  WordParam (fromIntegral (matrixOffset mb `div` 4)), 
                  WordParam (fromIntegral ldb), 
                  BufferParam (matrixBuffer mb)
               ]
            lda = ((matrixWidth ma) * 4 + (matrixPadding ma)) `div` 4
            ldb = ((matrixWidth mb) * 4 + (matrixPadding mb)) `div` 4


----------------------------------------
-- Kernels
---------------------------------------

kernels :: IO [Kernel]
kernels = initKernelsIO [
      CLKernel <$> CL.kernel
   ]
