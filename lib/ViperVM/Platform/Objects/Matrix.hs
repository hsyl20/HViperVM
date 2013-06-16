module ViperVM.Platform.Objects.Matrix (
   Matrix,
   createMatrix, matrixBuffer, matrixRegion, matrixCellType,
   matrixWidth, matrixHeight, matrixDimensions, matrixPadding, matrixOffset
) where

import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.Primitive

import Data.Word

-- | Matrix object
data Matrix = Matrix {
      matrixBuffer :: Buffer,
      matrixRegion :: Region,
      matrixCellType :: Primitive
   } deriving (Show,Eq,Ord)

-- | Create a matrix 
createMatrix :: Buffer -> Region -> Primitive -> Matrix
createMatrix b r@(Region2D {}) p = Matrix b r p
createMatrix _ _ _ = error "Invalid region (should be 2D)"

-- | Width of the matrix
matrixWidth :: Matrix -> Word64
matrixWidth = fst . matrixDimensions 

-- | Height of the matrix
matrixHeight :: Matrix -> Word64
matrixHeight = snd . matrixDimensions

-- | Dimensions of the matrix
matrixDimensions :: Matrix -> (Word64,Word64)
matrixDimensions m = (width `div` ps,height)
   where
      (Region2D _ height width _) = matrixRegion m
      ps = sizeOf (matrixCellType m)
   
-- | Padding of the matrix
matrixPadding :: Matrix -> Word64
matrixPadding m = let (Region2D _ _ _ p) = matrixRegion m in p

-- | Offset of the matrix
matrixOffset :: Matrix -> Word64
matrixOffset m = let (Region2D o _ _ _) = matrixRegion m in o
