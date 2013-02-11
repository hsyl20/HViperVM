module ViperVM.Runtime.Data where

import ViperVM.Runtime.Nodes
import qualified ViperVM.Platform as Pf

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (when,void)
import Data.Traversable
import Data.Maybe
import qualified ViperVM.STM.TSet as TSet


-- | Create a data instance node
createDataInstance :: Memory -> [(Pf.Buffer,Pf.Region)] -> STM DataInstance
createDataInstance mem regs = DataInstance mem regs <$> newTVar Nothing <*> TSet.empty <*> newTVar Nothing

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

