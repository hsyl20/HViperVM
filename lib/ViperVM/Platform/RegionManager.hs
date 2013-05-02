module ViperVM.Platform.RegionManager where

import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import qualified ViperVM.Platform.BufferManager as BM
import ViperVM.Platform.Region
import ViperVM.STM.TSet as TSet
import ViperVM.STM.TMap as TMap

import Data.Word
import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Data.Foldable (forM_)

data ManagerResult = Success | RegionAlreadyLocked

data BufferReleaseResult = BufferReleaseSuccess | RemainingRegionError

data LockMode = ReadOnly | ReadWrite deriving (Eq,Ord)
data LockedRegion = LockedRegion Region LockMode deriving (Eq,Ord)

data RegionManager = RegionManager {
                        bufferManager :: BM.BufferManager,
                        lockedRegions :: TMap Buffer (TSet LockedRegion)
                     }

-- | Initialize a region manager
createRegionManager :: BM.BufferManager -> IO RegionManager
createRegionManager b = atomically (RegionManager b <$> TMap.empty)

-- | Allocate a buffer in memory with region support)
allocateBuffer :: RegionManager -> Memory -> Word64 -> IO (Maybe Buffer)
allocateBuffer mg m sz = do
   buf <- BM.allocateBuffer (bufferManager mg) m sz
   forM_ buf $ \b -> 
      atomically (TMap.insert_ (lockedRegions mg) b =<< TSet.empty)
   return buf
      

-- | Release a buffer if no locked region remains
releaseBuffer :: RegionManager -> Buffer -> IO BufferReleaseResult
releaseBuffer mg b = do
   n <- atomically $ (TSet.null =<< bufferLockedRegions mg b)
   if not n
      then return RemainingRegionError
      else do
         BM.releaseBuffer (bufferManager mg) b
         return BufferReleaseSuccess


-- | Return locked regions of a buffer
bufferLockedRegions :: RegionManager -> Buffer -> STM (TSet LockedRegion)
bufferLockedRegions m b = lockedRegions m ! b
   
-- | Check if a region is locked
isLocked :: RegionManager -> Buffer -> Region -> LockMode -> STM Bool
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
lockRegion :: RegionManager -> Buffer -> Region -> LockMode -> STM ManagerResult
lockRegion m b r mode = do
   alreadyLocked <- isLocked m b r mode
   if alreadyLocked
      then return RegionAlreadyLocked
      else do
         TSet.insert (LockedRegion r mode) =<< bufferLockedRegions m b
         return Success

-- | Unlock a region
unlockRegion :: RegionManager -> Buffer -> Region -> LockMode -> STM ()
unlockRegion m b r mode = TSet.delete (LockedRegion r mode) =<< bufferLockedRegions m b


