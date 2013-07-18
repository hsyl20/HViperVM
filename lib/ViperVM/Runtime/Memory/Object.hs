{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.Runtime.Memory.Object (
   Object(..), Descriptor(..), objectMemory, checkObject,
   matrixDescDims, matrixDescWidth, matrixDescHeight
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Primitive
import ViperVM.Runtime.Memory.Objects.Vector
import ViperVM.Runtime.Memory.Objects.Matrix

import Data.Typeable
import Data.Word

-- | Descriptor of an object
data Descriptor = 
     VectorDesc Primitive Word64         -- ^ Vector
   | MatrixDesc Primitive Word64 Word64  -- ^ Matrix
   deriving (Eq,Ord,Show)

-- | An object
data Object = 
     VectorObject Vector   -- ^ Vector object
   | MatrixObject Matrix   -- ^ Matrix object
   deriving (Show,Eq,Ord,Typeable)


matrixDescDims :: Descriptor -> (Word64,Word64)
matrixDescDims (MatrixDesc _ w h) = (w,h)
matrixDescDims _ = error "Not a matrix descriptor"

matrixDescWidth :: Descriptor -> Word64
matrixDescWidth = fst . matrixDescDims

matrixDescHeight :: Descriptor -> Word64
matrixDescHeight = snd . matrixDescDims

matrixDescCellType :: Descriptor -> Primitive
matrixDescCellType (MatrixDesc p _ _) = p
matrixDescCellType _ = error "Not a matrix descriptor"

-- | Return the memory into which the object is stored
objectMemory :: Object -> Memory
objectMemory (VectorObject o) = bufferMemory (vectorBuffer o)
objectMemory (MatrixObject o) = bufferMemory (matrixBuffer o)


checkObject :: Descriptor -> Object -> Bool

checkObject (VectorDesc p sz) (VectorObject v) =
   vectorCellType v == p && vectorSize v == sz

checkObject (MatrixDesc p w h) (MatrixObject m) =
   matrixCellType m == p && matrixWidth m == w && matrixHeight m == h

checkObject _ _ = False
