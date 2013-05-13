module ViperVM.Platform.ObjectManager where

import ViperVM.Platform.Object
import ViperVM.Platform.Primitive
import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager

import Control.Monad
import Control.Applicative
import Data.Word

data ObjectManager = ObjectManager {
      regionLockManager :: RegionLockManager
   }

createObjectManager :: RegionLockManager -> IO ObjectManager
createObjectManager rm = return (ObjectManager rm)

allocateVector :: ObjectManager -> Memory -> Primitive -> Word64 -> IO (Maybe Vector)
allocateVector om mem p sz = do
   let rm = regionLockManager om
       bufferSize = sz * (sizeOf p)
       reg = Region1D 0 bufferSize
       makeVec buf = createVector buf reg p

   liftM (fmap makeVec) (allocateBuffer rm mem bufferSize)


allocateVectorObject :: ObjectManager -> Memory -> Primitive -> Word64 -> IO (Maybe Object)
allocateVectorObject om mem p sz = liftM VectorObject <$> allocateVector om mem p sz

allocateMatrix :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Padding -> IO (Maybe Matrix)
allocateMatrix om mem p width height padding = do
   let rm = regionLockManager om
       rowSize = width * (sizeOf p)
       bufferSize = (rowSize+padding) * height
       reg = Region2D 0 height rowSize padding
       makeMat buf = createMatrix buf reg p

   liftM (fmap makeMat) (allocateBuffer rm mem bufferSize)

allocateMatrixObject :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Padding -> IO (Maybe Object)
allocateMatrixObject om mem p width height padding = liftM MatrixObject <$> allocateMatrix om mem p width height padding
