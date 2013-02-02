module ViperVM.DataManager (
   DataManager,
   initDataManager,
   allocate,
   release,
   descriptor,
   datas
) where

import qualified Data.Map as Map
import Data.Map
import Data.Maybe
import Data.Word

import Control.Applicative

import ViperVM.Data

data DataManager = DataManager {
      descriptors :: Map Data DataDesc,
      lastId :: Word
   }

-- | Initialize a data manager
initDataManager :: DataManager
initDataManager = DataManager Map.empty 0

-- | Allocate a data
allocate :: DataManager -> DataDesc -> (DataManager, Data)
allocate manager desc = (newManager, dataId)
   where
      dataId = findFreeId manager
      newDescriptors = Map.insert dataId desc (descriptors  manager)
      newManager = DataManager newDescriptors dataId

-- | Find next free id
findFreeId :: DataManager -> Word
findFreeId manager = fromMaybe findNextFreeId $ const nextId <$> Map.lookup nextId (descriptors manager)
   where
      findNextFreeId = findFreeId (manager { lastId = nextId + 1 })
      nextId = lastId manager + 1

-- | Release a data
release :: DataManager -> Data -> DataManager
release manager d = DataManager newDescriptors (lastId manager)
   where
      newDescriptors = Map.delete d (descriptors manager)

-- | Retrieve data descriptor
descriptor :: DataManager -> Data -> DataDesc
descriptor manager d = fromJust $ Map.lookup d (descriptors manager)

-- | Retrieve data list
datas :: DataManager -> [Data]
datas manager = Map.keys (descriptors manager)
