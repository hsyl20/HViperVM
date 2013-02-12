module ViperVM.Runtime.Transfer where

import ViperVM.Runtime.Nodes
import qualified ViperVM.STM.TSet as TSet

import Control.Concurrent.STM
import Data.Maybe
import Control.Monad

-- | Create a data transfer node
createTransfer :: Data -> Link -> DataInstance -> DataInstance -> STM Transfer
createTransfer d l src dst = do
   let t = Transfer l src dst
   TSet.insert t (dataTransfers d)
   TSet.insert t (dataInstanceOutTransfers src)

   v <- readTVar $ dataInstanceInTransfer dst
   when (isJust v) $ error "Duplicated transfers targeting a data instance"
   writeTVar (dataInstanceInTransfer dst) (Just t)

   return t


-- | Delete a transfer node
releaseTransfer :: Data -> Transfer -> STM ()
releaseTransfer d t = do
   TSet.delete t (dataTransfers d)
   TSet.delete t (dataInstanceOutTransfers $ transferSource t)
   writeTVar (dataInstanceInTransfer $ transferTarget t) Nothing
