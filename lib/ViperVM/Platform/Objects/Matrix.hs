module ViperVM.Platform.Objects.Matrix (
   Matrix,
   createMatrix, matrixBuffer, matrixRegion, matrixCellType,
   matrixWidth, matrixHeight, matrixDimensions, matrixPadding, matrixOffset,
   matrixSubMatrix, matrixSubMatrixTrim, matrixSplit
) where

import ViperVM.Platform.Memory
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

-- | Split a matrix in smaller parts with the given dimensions
matrixSplit :: Matrix -> Word64 -> Word64 -> [[Matrix]]
matrixSplit m w h = [[makeMatrix x y | x <- [0..hn-1]] | y <- [0..vn-1]]
   where
      (gw,gh) = matrixDimensions m
      hn = (gw + (w-1)) `div` w
      vn = (gh + (h-1)) `div` h
      makeMatrix x y = matrixSubMatrixTrim m (x*w) (y*h) w h


-- | Return a sub matrix of the given size at the given offset 
matrixSubMatrix :: Matrix -> Word64 -> Word64 -> Word64 -> Word64 -> Matrix
matrixSubMatrix m x y w h = Matrix (matrixBuffer m) reg (matrixCellType m)
   where
      gw = matrixWidth m
      gpad = matrixPadding m
      goff = matrixOffset m
      ps = sizeOf (matrixCellType m)
      off = goff + x*ps + y*(gpad + gw*ps)
      pad = gpad + (gw - w) * ps
      reg = Region2D off h (w*ps) pad

-- | Return a sub matrix trimmed if too large
matrixSubMatrixTrim :: Matrix -> Word64 -> Word64 -> Word64 -> Word64 -> Matrix
matrixSubMatrixTrim m x y w h = matrixSubMatrix m x y w' h'
   where
      (gw,gh) = matrixDimensions m
      w' = if x+w > gw then gw-x else w
      h' = if y+h > gh then gh-y else h
