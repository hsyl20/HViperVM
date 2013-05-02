module ViperVM.Platform.TransferManager (
   TransferManager,
   createTransferManager, prepareTransfer, performTransfer, cancelTransfer, waitForTransfer, performDirectTransfer
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.RegionManager
import ViperVM.Platform.Transfer
import ViperVM.Platform.Link

import ViperVM.STM.TList as TList

import Data.Map
import Data.Traversable (forM)
import Data.Foldable(forM_)

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (foldM)

type TransferEvent = TVar (Maybe TransferResult)
data TransferStep = TransferStep Link Buffer Region

data TransferManager = TransferManager {
                           regionManager :: RegionManager,
                           transferWorkers :: Map Link (TList (Transfer, TransferEvent))
                       }

data PrepareTransferResult = PrepareSuccess | LockError [(Buffer,Region,RegionLockResult)]

-- | Initialize a new transfer manager
createTransferManager :: RegionManager -> IO TransferManager
createTransferManager rm = do
   let pf = getPlatform rm

   -- Create transfer threads
   threads <- forM (links pf) $ \link -> do
      trs <- atomically TList.empty
      _ <- forkOS (transferThread link trs)
      return (link, trs)
      
   return $ TransferManager rm (fromList threads)


-- | Thread that perform transfers on a given link
transferThread :: Link -> TList (Transfer, TransferEvent) -> IO ()
transferThread lnk trs = do
   (tr,ev) <- atomically $ do
      cond <- TList.null trs
      if cond then retry else pop trs

   err <- transfer tr
   atomically $ writeTVar ev (Just err)

   transferThread lnk trs
   

-- | Prepare a transfer
--
-- Lock every region involved
prepareTransfer :: TransferManager -> Buffer -> Region -> [TransferStep] -> STM PrepareTransferResult
prepareTransfer tm srcBuf srcReg steps = do
   let rm = regionManager tm

   -- Lock all regions
   r0 <- lockRegion rm srcBuf srcReg ReadOnly
   rs <- forM steps $ \(TransferStep _ b r) -> lockRegion rm b r ReadWrite
   let res0 = (srcBuf,srcReg,r0)
       res1 = Prelude.map (\(TransferStep _ b r, res) -> (b,r,res)) (steps `zip` rs)
       result = Prelude.filter (\(_,_,r) -> r /= LockSuccess) (res0:res1)

   return $ if not (Prelude.null result) then LockError result else PrepareSuccess


-- | Cancel a prepared transfer
--
-- Unlock every region involved
cancelTransfer :: TransferManager -> Buffer -> Region -> [TransferStep] -> STM ()
cancelTransfer tm srcBuf srcReg steps = do
   let rm = regionManager tm

   -- Unlock all regions
   unlockRegion rm srcBuf srcReg ReadOnly
   forM_ steps $ \(TransferStep _ b r) -> unlockRegion rm b r ReadWrite


-- | Perform a transfer synchronously
performTransfer :: TransferManager -> Buffer -> Region -> [TransferStep] -> IO [TransferResult]
performTransfer tm srcBuf srcReg steps = do
   evs <- atomically $ forM steps (const $ newTVar Nothing)
   (_,_,errs) <- foldM f (srcBuf,srcReg,[]) (steps `zip` evs)
   return (reverse errs)
   where
      f (b,r,errs) (TransferStep lnk tb tr, ev) = do
         if any (/= TransferSuccess) errs
            then return (b,r,errs)
            else do
               performDirectTransferEvent tm (Transfer lnk b r tb tr) ev
               err <- waitForTransfer tm ev
               return (tb,tr,err:errs)


performDirectTransfer :: TransferManager -> Transfer -> IO TransferEvent
performDirectTransfer tm t@(Transfer lnk _ _ _ _) = atomically $ do
   let workerSet = transferWorkers tm ! lnk
   ev <- newTVar Nothing
   TList.push workerSet (t,ev) 
   return ev

performDirectTransferEvent :: TransferManager -> Transfer -> TransferEvent -> IO ()
performDirectTransferEvent tm t@(Transfer lnk _ _ _ _) ev = atomically $ TList.push workerSet (t,ev)
   where
      workerSet = transferWorkers tm ! lnk

-- | Wait for a transfer to complete
waitForTransfer :: TransferManager -> TransferEvent -> IO TransferResult
waitForTransfer _ ev = atomically $ do
   res <- readTVar ev
   case res of
      Nothing -> retry
      Just r -> return r
