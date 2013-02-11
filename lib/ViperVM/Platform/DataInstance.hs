module ViperVM.Platform.DataInstance where

import Data.Word

data DataDesc = VectorDesc Primitive Word64
                deriving (Eq,Ord,Show)

backingBufferSize :: DataDesc -> Word64
backingBufferSize (VectorDesc PrimFloat n) = 4 * n
backingBufferSize (VectorDesc PrimDouble n) = 8 * n

data Primitive = PrimFloat | PrimDouble
                 deriving (Eq,Ord)

instance Show Primitive where
  show PrimFloat = "Float"
  show PrimDouble = "Double"

primitiveSize :: Primitive -> Word64
primitiveSize PrimFloat = 4
primitiveSize PrimDouble = 8
