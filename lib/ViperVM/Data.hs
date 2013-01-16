module ViperVM.Data where

import Data.Word
import ViperVM.Buffer
import ViperVM.Region

----------------------------------------------------

data Primitive = PrimFloat | PrimDouble
                 deriving (Eq,Ord)

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
                deriving (Eq,Ord,Show)

data DataInstance = Vector Region
                    deriving (Eq,Ord,Show)

getDataInstanceRegion :: DataInstance -> Region
getDataInstanceRegion (Vector v) = v

backingBufferSize :: DataDesc -> Word64
backingBufferSize (VectorDesc PrimFloat n) = 4 * n
backingBufferSize (VectorDesc PrimDouble n) = 8 * n

createDataInstance :: DataDesc -> Buffer -> DataInstance
createDataInstance desc@(VectorDesc _ _) b = Vector $ Region1D b 0 (backingBufferSize desc)
