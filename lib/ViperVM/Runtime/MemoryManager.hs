module ViperVM.Runtime.MemoryManager where

import ViperVM.Runtime.Nodes
import ViperVM.Runtime.Data
import ViperVM.STM.Common
import ViperVM.STM.TMap
import qualified ViperVM.STM.TMap as TMap
import ViperVM.STM.TSet
import Data.Set
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
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

   readyR :: TMap Data DataInstance,   -- ^ Pinned data instance ready for read access (already associated to the data)
   readyW :: TMap Data DataInstance,   -- ^ Pinned data instance ready for write access (not associated)
   readyRW :: TMap Data DataInstance   -- ^ Pinned data instance ready for read-write access (not associated, target data)
}

data DataManager = DataManager {
   pendingConfigs :: [DataConfig]
}

-- | Create a data config
createDataConfig :: Set ReadableConfig -> Set WritableConfig -> Set InplaceConfig -> STM DataConfig
createDataConfig r w rw = DataConfig <$> newTVar r <*> newTVar w <*> newTVar rw <*> TMap.empty <*> TMap.empty <*> TMap.empty


{-
-- | Instantiate a data configuration
makeDataConfig :: DataConfig -> IO ()
makeDataConfig conf = do
   -- We can already pin available data instances
   pinReady (dataR conf) (readyR conf) (/= ReadWrite)

   -- We need to allocate missing instances
   rds <- atomically $ readTVar (dataR conf)
   forM rds $ \(d,ms) -> do
      m <- head $ Set.elems ms
      desc <- atomically $ (fromJust <$> readTVar (dataDesc d))
      di <- allocateDataInstance m desc 
      -- Perform data transfers
      --TODO
      atomically $ TMap.insert d di readyR
      

   where
      pinReady dataSet readyMap fmode = atomically $ useTVar dataSet $ \rds -> do
         let rd = Set.elems rds
         dis <- forM rd $ \(d,ms) -> firstDataInstanceWithModeInMemories fmode ms d
         rmd <- forM (dis `zip` rd) $ \(mdi, (d, ms)) -> case mdi of
            Nothing -> return $ Just (d, ms)
            Just di -> do
               pinDataInstance ReadOnly di
               TMap.insert d di readyMap
               return Nothing
         return $ Set.fromList $ mapMaybe id rmd
-}
