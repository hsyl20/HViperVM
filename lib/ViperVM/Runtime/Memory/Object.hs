{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.Runtime.Memory.Object (
   Object(..), objectMemory
) where

import ViperVM.Platform.Memory
import ViperVM.Runtime.Memory.Objects.Vector
import ViperVM.Runtime.Memory.Objects.Matrix

import Data.Typeable

-- | An object
data Object = 
     VectorObject Vector   -- ^ Vector object
   | MatrixObject Matrix   -- ^ Matrix object
   deriving (Show,Eq,Ord,Typeable)

-- | Return the memory into which the object is stored
objectMemory :: Object -> Memory
objectMemory (VectorObject o) = bufferMemory (vectorBuffer o)
objectMemory (MatrixObject o) = bufferMemory (matrixBuffer o)

