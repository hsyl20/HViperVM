{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.Platform.Object (
   Object(..), objectMemory
) where

import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import ViperVM.Platform.Objects.Vector
import ViperVM.Platform.Objects.Matrix

import Data.Typeable

-- | An object
data Object = VectorObject Vector   -- ^ Vector object
            | MatrixObject Matrix   -- ^ Matrix object
              deriving (Show,Eq,Ord,Typeable)

-- | Return the memory into which the object is stored
objectMemory :: Object -> Memory
objectMemory (VectorObject o) = bufferMemory (vectorBuffer o)
objectMemory (MatrixObject o) = bufferMemory (matrixBuffer o)

