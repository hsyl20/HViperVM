module ViperVM.Platform.RegionLockManager (
   RegionLockManager, RegionLockResult(..), BufferReleaseResult(..), LockMode(..),
   createRegionLockManager, allocateBuffer, releaseBuffer, bufferLockedRegions, getRegionManagerPlatform,
   isLocked, lockRegion, lockRegions, unlockRegion, unlockRegions
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import qualified ViperVM.Platform.BufferManager as BM
import ViperVM.Platform.Region
import ViperVM.Platform.LockMode
import ViperVM.STM.TSet as TSet
import ViperVM.STM.TMap as TMap

import Data.Word
import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Control.Monad (forM)
import Data.Foldable (forM_)

data RegionLockResult = LockSuccess | RegionAlreadyLocked
                        deriving (Eq,Ord,Show)

data BufferReleaseResult = BufferReleaseSuccess | RemainingRegion
                           deriving (Eq,Ord,Show)

data LockedRegion = LockedRegion Region LockMode deriving (Eq,Ord)

data RegionLockManager = RegionLockManager {
                        bufferManager :: BM.BufferManager,
                        lockedRegions :: TMap Buffer (TSet LockedRegion)
                     }

-- | Initialize a region manager
createRegionLockManager :: BM.BufferManager -> IO RegionLockManager
createRegionLockManager b = atomically (RegionLockManager b <$> TMap.empty)

-- | Retrive platform used to create the buffer manager associated to this region manager
getRegionManagerPlatform :: RegionLockManager -> Platform
getRegionManagerPlatform rm = BM.getBufferManagerPlatform (bufferManager rm)

-- | Allocate a buffer in memory with region support)
allocateBuffer :: RegionLockManager -> Memory -> Word64 -> IO (Maybe Buffer)
allocateBuffer mg m sz = do
   buf <- BM.allocateBuffer (bufferManager mg) m sz
   forM_ buf $ \b -> 
      atomically (TMap.insert_ (lockedRegions mg) b =<< TSet.empty)
   return buf
      

-- | Release a buffer if no locked region remains
releaseBuffer :: RegionLockManager -> Buffer -> IO BufferReleaseResult
releaseBuffer mg b = do
   n <- atomically $ (TSet.null =<< bufferLockedRegions mg b)
   if not n
      then return RemainingRegion
      else do
         BM.releaseBuffer (bufferManager mg) b
         return BufferReleaseSuccess


-- | Return locked regions of a buffer
bufferLockedRegions :: RegionLockManager -> Buffer -> STM (TSet LockedRegion)
bufferLockedRegions m b = lockedRegions m ! b
   
-- | Check if a region is locked
isLocked :: RegionLockManager -> Buffer -> Region -> LockMode -> STM Bool
isLocked m b r mode = f . g <$> (TSet.toList =<< bufferLockedRegions m b)
   where 
      f = any (overlaps r) . map getLockRegion
      g = if mode == ReadWrite then id else Prelude.filter ((==) ReadWrite . getLockMode) 

-- | Retrieve lock mode
getLockMode :: LockedRegion -> LockMode
getLockMode (LockedRegion _ m) = m

-- | Retrieve lock region
getLockRegion :: LockedRegion -> Region
getLockRegion (LockedRegion r _) = r

-- | Lock a region or retry
lockRegion :: RegionLockManager -> LockMode -> Buffer -> Region -> STM RegionLockResult
lockRegion m mode b r = do
   alreadyLocked <- isLocked m b r mode
   if alreadyLocked
      then return RegionAlreadyLocked
      else do
         TSet.insert (LockedRegion r mode) =<< bufferLockedRegions m b
         return LockSuccess

-- | Lock several regions with the same mode
lockRegions :: RegionLockManager -> LockMode -> [(Buffer,Region)] -> STM [RegionLockResult]
lockRegions rm mode brs = forM brs (uncurry (lockRegion rm mode))

-- | Unlock a region
unlockRegion :: RegionLockManager -> LockMode -> Buffer -> Region -> STM ()
unlockRegion m mode b r = TSet.delete (LockedRegion r mode) =<< bufferLockedRegions m b


-- | Unlock several regions with the same mode
unlockRegions :: RegionLockManager -> LockMode -> [(Buffer,Region)] -> STM ()
unlockRegions rm mode brs = forM_ brs (uncurry (unlockRegion rm mode))
