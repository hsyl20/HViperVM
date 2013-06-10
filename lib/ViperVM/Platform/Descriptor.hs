module ViperVM.Platform.Descriptor where

import ViperVM.Platform.Primitive
import Data.Word

-- | Descriptor of an object
data Descriptor = VectorDesc Primitive Word64         -- ^ Vector
                | MatrixDesc Primitive Word64 Word64  -- ^ Matrix
                deriving (Eq,Ord,Show)

