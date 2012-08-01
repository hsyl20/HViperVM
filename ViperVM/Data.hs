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

data DataDesc = VectorDesc Primitive Word64
data DataInstance = Vector View

type DataID = Word
data Data = Data DataID DataDesc

instance Eq Data where
  (==) (Data id1 _) (Data id2 _) = id1 == id2

instance Ord Data where
  compare (Data id1 _) (Data id2 _) = compare id1 id2

dataDescriptor :: Data -> DataDesc
dataDescriptor (Data _ desc) = desc
