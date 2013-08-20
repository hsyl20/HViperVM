module ViperVM.Library.FloatMatrixAdd.Function (
   function, floatMatrixAddBuiltin
) where

import Control.Applicative ((<$>))
import ViperVM.VirtualPlatform.FunctionalKernel hiding (metaKernel,proto)
import ViperVM.VirtualPlatform.MetaObject
import ViperVM.VirtualPlatform.Descriptor

import ViperVM.Library.FloatMatrixAdd.MetaKernel

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

floatMatrixAddBuiltin :: MakeBuiltin
floatMatrixAddBuiltin = makeBuiltinIO function
