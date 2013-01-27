module ViperVM.RegionManager (
   RegionManager,
   ViperVM.RegionManager.init,
   add,
   remove,
   regions
) where

import Data.Map.Lazy
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.List as List

import ViperVM.Buffer
import ViperVM.Region

-- | Manage buffer regions
newtype RegionManager = RegionManager (Map Buffer [Region])

-- | Initialize a region manager
init :: RegionManager
init = RegionManager Map.empty

-- | Get regions 
regions :: RegionManager -> Buffer -> [Region]
regions (RegionManager m) b = fromMaybe [] (Map.lookup b m)

-- | Add a region
add :: RegionManager -> Buffer -> Region -> RegionManager
add (RegionManager m) b r = RegionManager $ Map.update (wrap . ((:) r )) b m
   where
      wrap s = if List.null s then Nothing else Just s

-- | Remove a region
remove :: RegionManager -> Buffer -> Region -> RegionManager
remove (RegionManager m) b r = RegionManager $ Map.update (wrap . (List.delete r)) b m
   where
      wrap s = if List.null s then Nothing else Just s
