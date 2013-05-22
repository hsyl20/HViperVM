module ViperVM.Platform.ObjectManager (
      ObjectManager, kernelManager,
      createObjectManager, releaseObject, lockObject, unlockObject, lockObjectRetry,
      allocateVector, allocateVectorObject, releaseVector,
      allocateMatrix, allocateMatrixObject, releaseMatrix
   ) where

import ViperVM.STM.TSet as TSet
import ViperVM.STM.TMap as TMap
import ViperVM.Platform.Object
import ViperVM.Platform.Primitive
import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager (RegionLockManager, LockMode(..), getRegionManagerPlatform, allocateBuffer)
import ViperVM.Platform.Platform
import ViperVM.Platform.KernelManager

import Control.Monad (when)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Control.Monad (liftM)
import Control.Applicative
import Data.Word
import Data.Map as Map

data ObjectLockResult = LockSuccess | LockError
                        deriving (Show,Eq,Ord)

data ObjectManager = ObjectManager {
      regionLockManager :: RegionLockManager,
      kernelManager :: KernelManager,
      objects :: Map Memory (TSet Object),
      locks :: TMap Object (LockMode,Int)
   }

createObjectManager :: RegionLockManager -> KernelManager -> IO ObjectManager
createObjectManager rm km = do
   let mems = memories (getRegionManagerPlatform rm)
   (objs,lcks) <- atomically $ do
      os <- forM mems (const TSet.empty)
      ls <- TMap.empty
      return (os,ls)
   let memObjs = fromList $ zip mems objs
   return (ObjectManager rm km memObjs lcks)


registerObject :: ObjectManager -> Memory -> Object -> STM ()
registerObject om mem obj = do
   let objs = objects om Map.! mem
   TSet.insert obj objs 


unregisterObject :: ObjectManager -> Memory -> Object -> STM ()
unregisterObject om mem obj = do
   let objs = objects om Map.! mem
   TSet.delete obj objs 

releaseObject :: ObjectManager -> Object -> IO ()
releaseObject om o = atomically $ do
   unregisterObject om (objectMemory o) o

lockObject :: ObjectManager -> LockMode -> Object -> STM ObjectLockResult
lockObject om mode o = do
   let lcks = locks om
   isMember <- TMap.member o lcks
   if not isMember
      then do
         TMap.insert o (mode,1) lcks
         return LockSuccess
      else do
         if mode == ReadWrite 
            then return LockError
            else do
               (currentMode,n) <- lcks TMap.! o
               if currentMode == ReadWrite
                  then return LockError
                  else do
                     TMap.insert o (currentMode, n+1) lcks
                     return LockSuccess

lockObjectRetry :: ObjectManager -> LockMode -> Object -> STM ()
lockObjectRetry om mode o = do
   r <- lockObject om mode o
   when (r /= LockSuccess) retry

unlockObject :: ObjectManager -> Object -> STM ()
unlockObject om o = do
   let lcks = locks om
   (currentMode,n) <- lcks TMap.! o
   if n == 1
      then
         TMap.delete o lcks
      else
         TMap.insert o (currentMode,n-1) lcks


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

------------------------------------------
-- Matrix
--

allocateMatrix :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Word64 -> IO (Maybe Matrix)
allocateMatrix om mem p width height padding = do
   let rm = regionLockManager om
       rowSize = width * (sizeOf p)
       bufferSize = (rowSize+padding) * height
       reg = Region2D 0 height rowSize padding
       makeMat buf = createMatrix buf reg p

   ret <- liftM (fmap makeMat) (allocateBuffer rm mem bufferSize)

   forM_ (MatrixObject <$> ret) (atomically . registerObject om mem)

   return ret

allocateMatrixObject :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Word64 -> IO (Maybe Object)
allocateMatrixObject om mem p width height padding = liftM MatrixObject <$> allocateMatrix om mem p width height padding

releaseMatrix :: ObjectManager -> Matrix -> IO ()
releaseMatrix om v = releaseObject om (MatrixObject v)
