module ViperVM.Runtime.MemoryManager where

import ViperVM.Runtime.Nodes
import ViperVM.STM.TMap
import qualified ViperVM.STM.TMap as TMap
import ViperVM.STM.TSet
import Data.Set
import Control.Applicative ( (<*>), (<$>) )
import Control.Concurrent.STM

type ReadableConfig = (Data, Set Memory)
type WritableConfig = (Data, Set Memory)
type InplaceConfig = (Data, Data, Set Memory)


-- | A data configuration that must be prepared by the data manager
-- Data instances must be prepared in one of the associated memory for each access mode
data DataConfig = DataConfig {
   dataR :: TSet ReadableConfig,   -- ^ Data to prepare for reading
   dataW :: TSet WritableConfig,   -- ^ Data to allocate for writing
   dataRW :: TSet InplaceConfig,   -- ^ Data to prepare for inplace modification

   readyR :: TMap Data DataInstance,   -- ^ Data instance ready for read access (already associated to the data)
   readyW :: TMap Data DataInstance,   -- ^ Data instance ready for write access (not associated)
   readyRW :: TMap Data DataInstance   -- ^ Data instance ready for read-write access (not associated, target data)
}

data DataManager = DataManager {
   pendingConfigs :: [DataConfig]
}

-- | Create a data config
createDataConfig :: Set ReadableConfig -> Set WritableConfig -> Set InplaceConfig -> STM DataConfig
createDataConfig r w rw = DataConfig <$> newTVar r <*> newTVar w <*> newTVar rw <*> TMap.empty <*> TMap.empty <*> TMap.empty
