module ViperVM.VirtualPlatform.Objects.Vector (
   Vector, vectorAllocate, vectorRelease, vectorTransfer,
   createVector, vectorBuffer, vectorRegion, vectorCellType, vectorSize,
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Primitive
import ViperVM.Platform.Link
import ViperVM.Common.Region
import ViperVM.Backends.Common.Buffer

import Control.Monad (liftM)
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
      
-- | Try to allocate a vector
vectorAllocate :: Memory -> Primitive -> Word64 -> IO (AllocResult Vector)
vectorAllocate mem p sz = do
   let
      bSize = sz * (sizeOf p)
      reg = Region1D 0 bSize
      makeVec buf = createVector buf reg p

   liftM (fmap makeVec) (bufferAllocate bSize mem)

-- | Release a vector
vectorRelease :: Vector -> IO ()
vectorRelease v = bufferRelease (vectorBuffer v)


-- | Transfer a vector
vectorTransfer :: Link -> Vector -> Vector -> IO ()
vectorTransfer link src dst = linkTransfer link srcBuffer srcRegion dstBuffer dstRegion
   where
      srcBuffer = vectorBuffer src
      dstBuffer = vectorBuffer dst
      srcRegion = vectorRegion src
      dstRegion = vectorRegion dst
