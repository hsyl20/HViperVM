module ViperVM.Runtime.Data where

import ViperVM.Runtime.Nodes
import ViperVM.STM.Common
import qualified ViperVM.Platform as Pf
import qualified ViperVM.Platform.BufferManager as Pf

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (when, void)
import Control.Monad.Loops
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
createDataInstance mem regs = DataInstance mem regs <$> newTVar 0 <*> newTVar NoAccess <*> newTVar Nothing <*> TSet.empty <*> newTVar Nothing

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

-- | Try to retrieve the first data instance with a valid access mode in the given memories
firstDataInstanceWithModeInMemories :: (AccessMode -> Bool) -> Set Memory -> Data -> STM (Maybe DataInstance)
firstDataInstanceWithModeInMemories mode ms d = firstM (\x -> mode <$> readTVar (dataInstanceMode x)) =<< (elems <$> dataInstancesInMemories ms d)

-- | Pin a data instance
pinDataInstance :: AccessMode -> DataInstance -> STM ()
pinDataInstance mode di = do
   writeTVar (dataInstanceMode di) mode
   withTVar (+1) (dataInstancePinCount di)

-- | Unpin a data instance
unpinDataInstance :: DataInstance -> STM ()
unpinDataInstance di = do
   withTVar (\x -> x - 1) (dataInstancePinCount di)
   n <- readTVar (dataInstancePinCount di)
   when (n == 0) $ writeTVar (dataInstanceMode di) NoAccess

-- | Allocate a data instance in a memory
allocateDataInstance :: Pf.BufferManager -> Memory -> Pf.DataDesc -> IO (Maybe DataInstance)
allocateDataInstance mm m desc = do
   let sz = Pf.backingBufferSize desc
       mem = memPeer m
   
   maybeBuf <- Pf.allocateBuffer mm mem sz

   case maybeBuf of
      Just buf -> do
         let reg = Pf.defaultRegion desc
         di <- atomically $ createDataInstance m [(buf,reg)]
         return (Just di)
      Nothing -> return Nothing

   
