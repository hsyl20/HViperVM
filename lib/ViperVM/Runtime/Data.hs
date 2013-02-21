module ViperVM.Runtime.Data where

import ViperVM.Runtime.Nodes
import qualified ViperVM.Platform as Pf

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (when,void)
import Data.Traversable
import Data.Maybe
import Data.Set
import qualified Data.Set as Set
import qualified ViperVM.STM.TSet as TSet

-- | Create a new data
createData :: Runtime -> Maybe Pf.DataDesc -> STM Data
createData r desc = do
   lstId <- readTVar (lastDataId r)
   writeTVar (lastDataId r) (lstId+1)  -- FIXME: potential overflow
   Data lstId <$> newTVar desc <*> TSet.empty <*> TSet.empty


-- | Create a data instance node
createDataInstance :: Memory -> [(Pf.Buffer,Pf.Region)] -> STM DataInstance
createDataInstance mem regs = DataInstance mem regs <$> newTVar 0 <*> newTVar Nothing <*> TSet.empty <*> newTVar Nothing

-- | Attach a data instance to a data
attachDataInstance :: Data -> DataInstance -> STM ()
attachDataInstance d di = do

   oldDat <- readTVar (dataInstanceData di)
   when (isJust oldDat) $ error "Trying to attach a data instance already attached"

   writeTVar (dataInstanceData di) (Just d)
   TSet.insert di (dataInstances d)


-- | Detach a data instance from a data
detachDataInstance :: DataInstance -> STM ()
detachDataInstance di = do
   oldDat <- readTVar (dataInstanceData di)
   void $ forM oldDat $ \d -> TSet.delete di (dataInstances d)
   writeTVar (dataInstanceData di) Nothing


-- | Retrieve instances of a data in a memory
dataInstancesInMemory :: Memory -> Data -> STM (Set DataInstance)
dataInstancesInMemory m d = Set.filter (\di -> dataInstanceMem di == m) <$> readTVar (dataInstances d)

-- | Retrieve instances of a data in one of the memories
dataInstancesInMemories :: Set Memory -> Data -> STM (Set DataInstance)
dataInstancesInMemories ms d = Set.filter (\di -> Set.member (dataInstanceMem di) ms) <$> readTVar (dataInstances d)
