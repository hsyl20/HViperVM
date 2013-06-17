module ViperVM.Platform.Descriptor where

import ViperVM.Platform.Primitive
import Data.Word

-- | Descriptor of an object
data Descriptor = VectorDesc Primitive Word64         -- ^ Vector
                | MatrixDesc Primitive Word64 Word64  -- ^ Matrix
                deriving (Eq,Ord,Show)

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
