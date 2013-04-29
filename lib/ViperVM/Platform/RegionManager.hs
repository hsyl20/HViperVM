module ViperVM.Platform.RegionManager where

import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.STM.TSet as TSet

import Control.Concurrent.STM
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (when)

data RegionManager = RegionManager {
                        buffer :: Buffer,
                        regions :: TSet Region,
                        lockedRegions :: TSet Region
                     }


-- | Initialize a region manager
createRegionManager :: Buffer -> IO RegionManager
createRegionManager b = atomically (RegionManager b <$> TSet.empty <*> TSet.empty)
   
-- | Register a region
registerRegion :: RegionManager -> Region -> STM ()
registerRegion m r = do
   TSet.insert r (regions m)

-- | Unregister a region
unregisterRegion :: RegionManager -> Region -> STM ()
unregisterRegion m r = do
   TSet.delete r (regions m)

-- | Lock a region or retry
lockRegion :: RegionManager -> Region -> STM ()
lockRegion m r = do
   alreadyLocked <- TSet.member r (lockedRegions m)
   when (alreadyLocked) retry
   TSet.insert r (lockedRegions m)

-- | Unlock a region
unlockRegion :: RegionManager -> Region -> STM ()
unlockRegion m r = do
   TSet.delete r (lockedRegions m)


