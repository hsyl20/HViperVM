module ViperVM.Platform.DataDesc where

import Data.Word
import ViperVM.Platform.Region
import qualified ViperVM.Platform.Primitive as Prim
import ViperVM.Platform.Primitive (Primitive)

data DataDesc = VectorDesc Primitive Word64
                deriving (Eq,Ord,Show)

backingBufferSize :: DataDesc -> Word64
backingBufferSize (VectorDesc Prim.Float n) = 4 * n
backingBufferSize (VectorDesc Prim.Double n) = 8 * n


defaultRegion :: DataDesc -> Region
defaultRegion (VectorDesc p n) = Region1D 0 (n * Prim.sizeOf p)

