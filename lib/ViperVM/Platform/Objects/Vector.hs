module ViperVM.Platform.Objects.Vector (
   Vector,
   createVector, vectorBuffer, vectorRegion, vectorCellType, vectorSize,
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.Primitive

import Data.Word

-- | Vector object
data Vector = Vector {
      vectorBuffer :: Buffer,
      vectorRegion :: Region,
      vectorCellType :: Primitive
   } deriving (Show,Eq,Ord)

-- | Create a vector
createVector :: Buffer -> Region -> Primitive -> Vector
createVector b r@(Region1D {}) p = Vector b r p
createVector _ _ _ = error "Invalid region (should be 1D)"

-- | Return the size of the vector
vectorSize :: Vector -> Word64
vectorSize v = s `div` ps
   where
      (Region1D _ s) = vectorRegion v
      ps = sizeOf (vectorCellType v)
      

