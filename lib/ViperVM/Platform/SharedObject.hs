module ViperVM.Platform.SharedObject where

import ViperVM.Platform.Object
import ViperVM.STM.TSet

import ViperVM.Platform.Primitive
import Data.Word

data Descriptor = VectorDesc Primitive Word64 
                | MatrixDesc Primitive Word64 Word64
                deriving (Eq,Ord,Show)

