module ViperVM.Data where

import Data.Word
import ViperVM.View
import ViperVM.Buffer
import ViperVM.Platform

----------------------------------------------------

data Primitive = PrimFloat | PrimDouble

instance Show Primitive where
  show PrimFloat = "Float"
  show PrimDouble = "Double"

primitiveSize :: Primitive -> Word64
primitiveSize PrimFloat = 4
primitiveSize PrimDouble = 8

----------------------------------------------------

type DataID = Word
data Data = Data DataID DataDesc
            deriving (Show)

instance Eq Data where
  (==) (Data id1 _) (Data id2 _) = id1 == id2

instance Ord Data where
  compare (Data id1 _) (Data id2 _) = compare id1 id2

dataDescriptor :: Data -> DataDesc
dataDescriptor (Data _ desc) = desc

----------------------------------------------------

data DataDesc = VectorDesc Primitive Word64
                deriving (Show)

data DataInstance = Vector View

-- | Return memory containing a data instance
getDataInstanceMemory :: DataInstance -> Memory
getDataInstanceMemory (Vector (View1D buf _ _)) = getBufferMemory buf
getDataInstanceMemory _ = undefined
