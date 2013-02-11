module ViperVM.Runtime.Data where

import Data.Word
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.Link
import ViperVM.STM.TSet

import Control.Concurrent.STM.TVar


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

data DataDesc = VectorDesc Primitive Word64
                deriving (Eq,Ord,Show)

data DataInstance = Vector Buffer Region
                    deriving (Eq,Ord,Show)

getDataInstanceRegion :: DataInstance -> Region
getDataInstanceRegion (Vector _ r) = r

getDataInstanceBuffer :: DataInstance -> Buffer
getDataInstanceBuffer (Vector b _) = b

backingBufferSize :: DataDesc -> Word64
backingBufferSize (VectorDesc PrimFloat n) = 4 * n
backingBufferSize (VectorDesc PrimDouble n) = 8 * n

createDataInstance :: DataDesc -> Buffer -> DataInstance
createDataInstance desc@(VectorDesc _ _) b = Vector b $ Region1D 0 (backingBufferSize desc)

