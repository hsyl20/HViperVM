module ViperVM.Platform.DataDesc where

import Data.Word
import ViperVM.Platform.Region
import ViperVM.Platform.Primitive

data DataDesc = VectorDesc Primitive Word64
                deriving (Eq,Ord,Show)

backingBufferSize :: DataDesc -> Word64
backingBufferSize (VectorDesc PrimFloat n) = 4 * n
backingBufferSize (VectorDesc PrimDouble n) = 8 * n


defaultRegion :: DataDesc -> Region
defaultRegion (VectorDesc p n) = Region1D 0 (n * primitiveSize p)

