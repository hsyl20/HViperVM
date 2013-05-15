module ViperVM.Platform.ObjectManager (
      createObjectManager, releaseObject, lockObject, unlockObject,
      allocateVector, allocateVectorObject, releaseVector, lockVector, unlockVector,
      allocateMatrix, allocateMatrixObject, releaseMatrix, lockMatrix, unlockMatrix
   ) where

import ViperVM.STM.TSet as TSet
import ViperVM.Platform.Object
import ViperVM.Platform.Primitive
import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.Platform

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Control.Monad (liftM)
import Control.Applicative
import Data.Word
import Data.Map

data ObjectManager = ObjectManager {
      regionLockManager :: RegionLockManager,
      objects :: Map Memory (TSet Object)
   }

createObjectManager :: RegionLockManager -> IO ObjectManager
createObjectManager rm = do
   let mems = memories (getPlatform rm)
   objs <- atomically $ forM mems (const TSet.empty)
   let memObjs = fromList $ zip mems objs
   return (ObjectManager rm memObjs)


registerObject :: ObjectManager -> Memory -> Object -> STM ()
registerObject om mem obj = do
   let objs = objects om ! mem
   TSet.insert obj objs 


unregisterObject :: ObjectManager -> Memory -> Object -> STM ()
unregisterObject om mem obj = do
   let objs = objects om ! mem
   TSet.delete obj objs 

releaseObject :: ObjectManager -> Object -> IO ()
releaseObject om o = do
   atomically $ unregisterObject om (objectMemory o) o

lockObject :: ObjectManager -> LockMode -> Object -> IO ()
lockObject om mode (VectorObject v) = lockVector om mode v
lockObject om mode (MatrixObject m) = lockMatrix om mode m

unlockObject :: ObjectManager -> LockMode -> Object -> IO ()
unlockObject om mode (VectorObject v) = unlockVector om mode v
unlockObject om mode (MatrixObject m) = unlockMatrix om mode m

------------------------------------------
-- Vector
--

allocateVector :: ObjectManager -> Memory -> Primitive -> Word64 -> IO (Maybe Vector)
allocateVector om mem p sz = do
   let rm = regionLockManager om
       bufferSize = sz * (sizeOf p)
       reg = Region1D 0 bufferSize
       makeVec buf = createVector buf reg p

   ret <- liftM (fmap makeVec) (allocateBuffer rm mem bufferSize)

   forM_ (VectorObject <$> ret) (atomically . registerObject om mem)

   return ret


allocateVectorObject :: ObjectManager -> Memory -> Primitive -> Word64 -> IO (Maybe Object)
allocateVectorObject om mem p sz = liftM VectorObject <$> allocateVector om mem p sz

releaseVector :: ObjectManager -> Vector -> IO ()
releaseVector om v = releaseObject om (VectorObject v)

lockVector :: ObjectManager -> LockMode -> Vector -> IO ()
lockVector om mode v = void $ atomically $ lockRegion rm mode (vectorBuffer v) (vectorRegion v)
   where
      rm = regionLockManager om
   
unlockVector :: ObjectManager -> LockMode -> Vector -> IO ()
unlockVector om mode v = atomically $ unlockRegion rm mode (vectorBuffer v) (vectorRegion v)
   where
      rm = regionLockManager om

------------------------------------------
-- Matrix
--

allocateMatrix :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Padding -> IO (Maybe Matrix)
allocateMatrix om mem p width height padding = do
   let rm = regionLockManager om
       rowSize = width * (sizeOf p)
       bufferSize = (rowSize+padding) * height
       reg = Region2D 0 height rowSize padding
       makeMat buf = createMatrix buf reg p

   ret <- liftM (fmap makeMat) (allocateBuffer rm mem bufferSize)

   forM_ (MatrixObject <$> ret) (atomically . registerObject om mem)

   return ret

allocateMatrixObject :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Padding -> IO (Maybe Object)
allocateMatrixObject om mem p width height padding = liftM MatrixObject <$> allocateMatrix om mem p width height padding

releaseMatrix :: ObjectManager -> Matrix -> IO ()
releaseMatrix om v = releaseObject om (MatrixObject v)

lockMatrix :: ObjectManager -> LockMode -> Matrix -> IO ()
lockMatrix om mode v = void $ atomically $ lockRegion rm mode (matrixBuffer v) (matrixRegion v)
   where
      rm = regionLockManager om

unlockMatrix :: ObjectManager -> LockMode -> Matrix -> IO ()
unlockMatrix om mode v = atomically $ unlockRegion rm mode (matrixBuffer v) (matrixRegion v)
   where
      rm = regionLockManager om
