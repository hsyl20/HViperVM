module ViperVM.Platform.TransferManager where

import ViperVM.Platform.Platform
import ViperVM.Platform.Buffer as Buffer
import ViperVM.Platform.BufferManager
import ViperVM.Platform.RegionManager
import ViperVM.Platform.Transfer
import ViperVM.Platform.Link

import ViperVM.STM.TMap as TMap
import ViperVM.STM.TList as TList

import Data.Map
import Data.Traversable (forM)

import Control.Concurrent.STM
import Control.Concurrent

type TransferEvent = TVar (Maybe TransferResult)

data TransferManager = TransferManager {
                           platform :: Platform,
                           bufferManager :: BufferManager,
                           regionManagers :: TMap Buffer RegionManager,
                           transferWorkers :: Map Link (TList (Transfer, TransferEvent))
                       }

-- | Initialize a new transfer manager
createTransferManager :: Platform -> IO TransferManager
createTransferManager pf = do
   bufMgr <- createBufferManager pf
   regMgr <- atomically TMap.empty

   -- Create transfer threads
   threads <- forM (links pf) $ \link -> do
      trs <- atomically TList.empty
      _ <- forkOS (transferThread link trs)
      return (link, trs)
      

   return $ TransferManager pf bufMgr regMgr (fromList threads)


-- | Thread that perform transfers on a given link
transferThread :: Link -> TList (Transfer, TransferEvent) -> IO ()
transferThread lnk trs = do
   (tr,ev) <- atomically $ do
      cond <- TList.null trs
      if cond then retry else pop trs

   err <- transfer tr
   atomically $ writeTVar ev (Just err)

   transferThread lnk trs
   

-- | Submit a direct transfer
submitDirectTransfer :: TransferManager -> Transfer -> IO TransferEvent
submitDirectTransfer tm t@(Transfer lnk _ _ _ _) = atomically $ do
   let workerSet = transferWorkers tm ! lnk
   ev <- newTVar Nothing
   TList.push workerSet (t,ev) 
   return ev

waitForTransfer :: TransferManager -> TransferEvent -> IO TransferResult
waitForTransfer tm ev = atomically $ do
   res <- readTVar ev
   case res of
      Nothing -> retry
      Just r -> return r
