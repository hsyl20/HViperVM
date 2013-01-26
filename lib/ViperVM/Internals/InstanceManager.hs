module ViperVM.Internals.DataManager (
   ViperVM.Internals.DataManager.init,
   allocate,
   associate,
   dissociate,
   dataInstances,
   dataDescriptor
) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map
import Data.Word
import Data.Maybe

import ViperVM.Data

data DataManager = DataManager {
   descriptors :: Map Data DataDesc,
   instances :: Map Data [DataInstance],
   lastId :: Word
}

-- | Initialize a data manager
init :: DataManager
init = DataManager {
      descriptors = Map.empty,
      instances = Map.empty,
      lastId = 0
   }


-- | Allocate a data
allocate :: DataManager -> DataDesc -> (DataManager, Data)
allocate manager desc = (newManager, dataId)
   where
      dataId = lastId manager
      newDescriptors = Map.insert dataId desc (descriptors  manager)
      newManager = DataManager newDescriptors (instances manager) (dataId + 1)

-- | Associate an instance to a data
associate :: DataManager -> Data -> DataInstance -> DataManager
associate manager d inst = manager { instances = Map.update (wrap . ((:) inst)) d (instances manager) }
   where
      wrap s = if List.null s then Nothing else Just s

-- | Dissociate an instance from a data
dissociate :: DataManager -> Data -> DataInstance -> DataManager
dissociate manager d inst = manager { instances = Map.update (wrap . (List.delete inst)) d (instances manager) }
   where
      wrap s = if List.null s then Nothing else Just s

-- | Return instances of a data
dataInstances :: DataManager -> Data -> [DataInstance]
dataInstances manager d = fromMaybe [] (Map.lookup d (instances manager))

-- | Return data descriptor
dataDescriptor :: DataManager -> Data -> DataDesc
dataDescriptor manager d = fromJust (Map.lookup d (descriptors manager))
