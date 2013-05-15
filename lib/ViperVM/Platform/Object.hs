module ViperVM.Platform.Object (
      Object(..), Vector, Matrix,
      objectMemory,
      createVector, vectorBuffer, vectorRegion, vectorCellType, vectorSize,
      createMatrix, matrixBuffer, matrixRegion, matrixCellType,
      matrixWidth, matrixHeight, matrixDimensions
   ) where

import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.Memory
import ViperVM.Platform.Primitive

import Data.Word

data Object = VectorObject Vector |
              MatrixObject Matrix
              deriving (Show,Eq,Ord)

objectMemory :: Object -> Memory
objectMemory (VectorObject o) = getBufferMemory (vectorBuffer o)
objectMemory (MatrixObject o) = getBufferMemory (matrixBuffer o)

data Vector = Vector {
      vectorBuffer :: Buffer,
      vectorRegion :: Region,
      vectorCellType :: Primitive
   } deriving (Show,Eq,Ord)

createVector :: Buffer -> Region -> Primitive -> Vector
createVector b r@(Region1D {}) p = Vector b r p
createVector _ _ _ = error "Invalid region (should be 1D)"

vectorSize :: Vector -> Word64
vectorSize v = s `div` ps
   where
      (Region1D _ s) = vectorRegion v
      ps = sizeOf (vectorCellType v)
      

data Matrix = Matrix {
      matrixBuffer :: Buffer,
      matrixRegion :: Region,
      matrixCellType :: Primitive
   } deriving (Show,Eq,Ord)

createMatrix :: Buffer -> Region -> Primitive -> Matrix
createMatrix b r@(Region2D {}) p = Matrix b r p
createMatrix _ _ _ = error "Invalid region (should be 2D)"

matrixWidth :: Matrix -> Word64
matrixWidth = fst . matrixDimensions 

matrixHeight :: Matrix -> Word64
matrixHeight = snd . matrixDimensions

matrixDimensions :: Matrix -> (Word64,Word64)
matrixDimensions m = (width `div` ps,height)
   where
      (Region2D _ height width _) = matrixRegion m
      ps = sizeOf (matrixCellType m)
   
