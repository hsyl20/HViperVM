module ViperVM.RegionManager (
   RegionManager, AccessMode(..),
   initRegionManager,
   lock, unlock, regions
) where

import Data.Map.Lazy
import qualified Data.List as List
import qualified Data.Map.Lazy as Map

import ViperVM.Buffer
import ViperVM.Region

data AccessMode = ReadOnly | ReadWrite | WriteOnly
                  deriving (Eq,Ord)

data LockedRegion = LockedRegion {
      region :: Region,          -- ^ Region of a locked region
      accessMode :: AccessMode  -- ^ Access mode of a locked region
   } deriving (Eq,Ord)

-- | Manage buffer regions
newtype RegionManager = RegionManager (Map Buffer [LockedRegion])

-- | Initialize a region manager
initRegionManager :: RegionManager
initRegionManager = RegionManager Map.empty

-- | Retrive all regions 
regions :: RegionManager -> Buffer -> [Region]
regions (RegionManager m) b = List.map region $ Map.findWithDefault [] b m

-- | Retrieve write-access regions
writeRegions :: RegionManager -> Buffer -> [Region]
writeRegions (RegionManager m) b = List.map region . List.filter ((/=) ReadOnly . accessMode) $ Map.findWithDefault [] b m

-- | Lock a region
lock :: RegionManager -> Buffer -> Region -> AccessMode -> (RegionManager, Bool)
lock mgr@(RegionManager m) b r mode = (mgr2, not overlapping)
   where
      concurrents = concurrentRegions mgr b mode
      overlapping = not . List.null $ r `overlapsAny` concurrents

      f = if overlapping then id else (:) (LockedRegion r mode)

      mgr2 = RegionManager $ Map.update (wrap . f) b m
      wrap s = if List.null s then Nothing else Just s


-- | Retrieve regions of the same buffer that use a concurrent access mode (R vs W)
concurrentRegions :: RegionManager -> Buffer -> AccessMode -> [Region]
concurrentRegions mgr buffer ReadOnly = writeRegions mgr buffer
concurrentRegions mgr buffer _ = regions mgr buffer


-- | Unlock a region
unlock :: RegionManager -> Buffer -> Region -> AccessMode -> RegionManager
unlock (RegionManager m) b r mode = RegionManager $ Map.update (wrap . (List.delete (LockedRegion r mode))) b m
   where
      wrap s = if List.null s then Nothing else Just s
