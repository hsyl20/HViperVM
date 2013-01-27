module ViperVM.InstanceManager (
   InstanceManager,
   ViperVM.InstanceManager.init,
   associate,
   dissociate,
   instances,
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map
import Data.Maybe

import ViperVM.Data

-- | Manage instances of a data
newtype InstanceManager = InstanceManager (Map Data [DataInstance])

-- | Initialize an instance manager
init :: InstanceManager
init = InstanceManager Map.empty

-- | Return instances of a data
instances :: InstanceManager -> Data -> [DataInstance]
instances (InstanceManager m) d = fromMaybe [] (Map.lookup d m)

-- | Associate an instance to a data
associate :: InstanceManager -> Data -> DataInstance -> InstanceManager
associate (InstanceManager m) d inst = InstanceManager $ Map.update (wrap . ((:) inst)) d m
   where
      wrap s = if List.null s then Nothing else Just s

-- | Dissociate an instance from a data
dissociate :: InstanceManager -> Data -> DataInstance -> InstanceManager
dissociate (InstanceManager m) d inst = InstanceManager $ Map.update (wrap . (List.delete inst)) d m
   where
      wrap s = if List.null s then Nothing else Just s
