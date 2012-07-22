module ViperVM.Data where

import Data.Word
import ViperVM.View

data Primitive = PrimFloat | PrimDouble

instance Show Primitive where
  show PrimFloat = "Float"
  show PrimDouble = "Double"

primitiveSize :: Primitive -> Word64
primitiveSize PrimFloat = 4
primitiveSize PrimDouble = 8

data VectorDesc = VectorDesc Primitive Word64

data DataInstance = Vector VectorDesc View

newtype Data = Data Word deriving (Eq,Ord)
