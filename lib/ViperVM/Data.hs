module ViperVM.Data where

import Data.Word
import ViperVM.View

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

data DataInstance = Vector View
                    deriving (Eq,Ord,Show)

-- | Allocate a data instance in the given memory
{-allocateDataInstance :: Data -> Memory -> Maybe DataInstance
allocateDataInstance d m = do-}
  
