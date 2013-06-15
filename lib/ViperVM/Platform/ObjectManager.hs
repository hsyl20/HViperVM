{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.ObjectManager (
      ObjectManager, kernelManager,
      createObjectManager, releaseObject, lockObject, unlockObject, lockObjectRetry, transferObject,
      allocateVector, allocateVectorObject, releaseVector,
      allocateMatrix, allocateMatrixObject, releaseMatrix,
      transferMatrix, pokeHostFloatMatrix, peekHostFloatMatrix,
      allocateFromDescriptor,
      getObjectManagerPlatform
   ) where

import ViperVM.STM.TSet as TSet
import ViperVM.STM.TMap as TMap
import ViperVM.Platform.Buffer
import ViperVM.Platform.Object
import ViperVM.Platform.Primitive as Prim
import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager (RegionLockManager, LockMode(..), allocateBuffer)
import qualified ViperVM.Platform.RegionTransferManager as TM
import ViperVM.Platform.Platform
import ViperVM.Platform.Link
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionTransfer
import ViperVM.Platform.Descriptor
import ViperVM.Platform.TransferResult
import ViperVM.Backends.Host.Buffer as Host

import Control.Monad (when)
import Control.Concurrent.STM
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Control.Monad (liftM)
import Control.Applicative
import Data.Word
import Data.Map as Map
import Foreign.Ptr
import Foreign.Marshal.Array
import Text.Printf

data ObjectLockResult = LockSuccess | LockError
                        deriving (Show,Eq,Ord)

data ObjectManager = ObjectManager {
      regionTransferManager :: TM.RegionTransferManager,
      kernelManager :: KernelManager,
      objects :: Map Memory (TSet Object),
      locks :: TMap Object (LockMode,Int)
   }

-- | Return associated platform
getObjectManagerPlatform :: ObjectManager -> Platform
getObjectManagerPlatform = TM.getRegionTransferManagerPlatform . regionTransferManager

regionLockManager :: ObjectManager -> RegionLockManager
regionLockManager = TM.regionLockManager . regionTransferManager

createObjectManager :: TM.RegionTransferManager -> KernelManager -> IO ObjectManager
createObjectManager tm km = do
   let
      mems = memories (TM.getRegionTransferManagerPlatform tm)

   (objs,lcks) <- atomically $ do
      os <- forM mems (const TSet.empty)
      ls <- TMap.empty
      return (os,ls)

   let memObjs = fromList $ zip mems objs
   return (ObjectManager tm km memObjs lcks)


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


transferObject :: ObjectManager -> Object -> Object -> IO ()
transferObject om (MatrixObject src) (MatrixObject dst) = do
   let pf = getObjectManagerPlatform om
   customLog pf (printf "[Transfer] %s -> %s" (show src) (show dst))
   transferMatrix om src dst
transferObject _ _ _ = error "Cannot transfer these objects"

------------------------------------------
-- Vector
--

allocateVector :: ObjectManager -> Memory -> Primitive -> Word64 -> IO (Maybe Vector)
allocateVector om mem p sz = do
   let rm = regionLockManager om
       bSize = sz * (sizeOf p)
       reg = Region1D 0 bSize
       makeVec buf = createVector buf reg p

   ret <- liftM (fmap makeVec) (allocateBuffer rm mem bSize)

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
       bSize = (rowSize+padding) * height
       reg = Region2D 0 height rowSize padding
       makeMat buf = createMatrix buf reg p

   ret <- liftM (fmap makeMat) (allocateBuffer rm mem bSize)

   forM_ (MatrixObject <$> ret) (atomically . registerObject om mem)

   return ret

allocateMatrixObject :: ObjectManager -> Memory -> Primitive -> Word64 -> Word64 -> Word64 -> IO (Maybe Object)
allocateMatrixObject om mem p width height padding = liftM MatrixObject <$> allocateMatrix om mem p width height padding

releaseMatrix :: ObjectManager -> Matrix -> IO ()
releaseMatrix om v = releaseObject om (MatrixObject v)

transferMatrix :: ObjectManager -> Matrix -> Matrix -> IO ()
transferMatrix om src dst = do
   let tm = regionTransferManager om 
       pf = TM.getRegionTransferManagerPlatform tm
       lks = links pf
       validLinks = linksBetween (bufferMemory srcBuf) (bufferMemory dstBuf) lks
       lk = head validLinks
       transfr = RegionTransfer srcBuf srcReg [RegionTransferStep lk dstBuf dstReg]
       srcBuf = matrixBuffer src
       dstBuf = matrixBuffer dst
       srcReg = matrixRegion src
       dstReg = matrixRegion dst

   when (Prelude.null validLinks)
      (error "Cannot transfer (no link found)")

   pres <- TM.prepareRegionTransferIO tm transfr
   when (pres /= TM.PrepareSuccess)
      (error "Error during matrix transfer preparation")
      
   
   res <- TM.performRegionTransfer tm transfr
   let res2 = Prelude.filter (/= RegionTransferSuccess) res
   when (not (Prelude.null res2))
      (error ("Error during matrix transfer: " ++ show res2))

pokeHostFloatMatrix :: ObjectManager -> Object -> [[Float]] -> IO ()
pokeHostFloatMatrix _ obj@(MatrixObject m) ds = do
   when (objectMemory obj /= HostMemory) 
      (error "Cannot initialize a matrix that is not stored in host memory")

   when (matrixCellType m /= Prim.Float) 
      (error "Cannot initialize the matrix: invalid cell type")

   let rowSize = matrixWidth m * Prim.sizeOf (matrixCellType m) + matrixPadding m
       HostBuffer buf = matrixBuffer m
       startPtr = Host.bufferPtr buf `plusPtr` (fromIntegral $ matrixOffset m)

   forM_ (ds `zip` [0..]) $ \(xs,i) -> do
      let ptr = startPtr `plusPtr` (fromIntegral $ i*rowSize)
      pokeArray (castPtr ptr) xs
         
pokeHostFloatMatrix _ _ _ = error "Cannot poke the given object"



peekHostFloatMatrix :: ObjectManager -> Object -> IO [[Float]]
peekHostFloatMatrix _ obj@(MatrixObject m) = do

   when (objectMemory obj /= HostMemory) 
      (error "Cannot initialize a matrix that is not stored in host memory")

   when (matrixCellType m /= Prim.Float) 
      (error "Cannot initialize the matrix: invalid cell type")

   let rowSize = matrixWidth m * Prim.sizeOf (matrixCellType m) + matrixPadding m
       HostBuffer buf = matrixBuffer m
       startPtr = Host.bufferPtr buf `plusPtr` (fromIntegral $ matrixOffset m)
       height = matrixHeight m
       width = fromIntegral (matrixWidth m)

   forM [0..height-1] $ \i -> do
      let ptr = startPtr `plusPtr` (fromIntegral $ i*rowSize)
      peekArray width (castPtr ptr)


peekHostFloatMatrix _ _ = error "Cannot peek the given object"




-- | Allocate a compatible instance of the shared object, DO NOT atach it
allocateFromDescriptor :: ObjectManager -> Memory -> Descriptor -> IO Object
allocateFromDescriptor om mem (MatrixDesc prim w h) = do
   let padding = (w * Prim.sizeOf prim) `mod` 4
   allocateMatrix om mem prim w h padding >>= \case
      Nothing -> error "Unable to allocate matrix"
      Just m -> return (MatrixObject m)

allocateFromDescriptor om mem (VectorDesc prim sz) = do
   allocateVector om mem prim sz >>= \case
      Nothing -> error "Unable to allocate vector"
      Just v -> return (VectorObject v)
