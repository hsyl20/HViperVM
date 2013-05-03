module ViperVM.Platform.Primitive where

import Data.Word

data Primitive = PrimFloat | PrimDouble
                 deriving (Eq,Ord)

instance Show Primitive where
  show PrimFloat = "Float"
  show PrimDouble = "Double"

primitiveSize :: Primitive -> Word64
primitiveSize PrimFloat = 4
primitiveSize PrimDouble = 8
