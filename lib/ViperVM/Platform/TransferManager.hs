module ViperVM.Platform.TransferManager (
   TransferManager, PrepareTransferResult(..),
   createTransferManager, prepareTransfer, performTransfer, cancelTransfer,
   prepareTransferIO
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.RegionManager
import ViperVM.Platform.Transfer
import ViperVM.Platform.Link
import ViperVM.Event

import Data.Map
import Data.Traversable (forM)
import Data.Foldable(forM_)

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (foldM)

type TransferEvent = Event TransferResult

data TransferManager = TransferManager {
                           regionManager :: RegionManager,
                           transferWorkers :: Map Link (TChan (DirectTransfer, TransferEvent))
                       }

data PrepareTransferResult = PrepareSuccess | 
                             LockError [(Buffer,Region,RegionLockResult)] |
                             InvalidTransfer
                             deriving (Eq,Ord)

-- | Initialize a new transfer manager
createTransferManager :: RegionManager -> IO TransferManager
createTransferManager rm = do
   let pf = getPlatform rm

   -- Create transfer threads
   threads <- forM (links pf) $ \link -> do
      trs <- atomically newTChan
      _ <- forkOS (transferThread link trs)
      return (link, trs)
      
   return $ TransferManager rm (fromList threads)


-- | Thread that perform transfers on a given link
transferThread :: Link -> TChan (DirectTransfer, TransferEvent) -> IO ()
transferThread lnk ch = do
   (tr,ev) <- atomically $ readTChan ch

   err <- transfer tr
   setEvent ev err

   transferThread lnk ch
   

-- | Prepare a transfer
--
-- Lock every region involved
prepareTransfer :: TransferManager -> Transfer -> STM PrepareTransferResult
prepareTransfer tm t@(Transfer srcBuf srcReg steps) = do
   let rm = regionManager tm

   if not (checkTransfer t)
      then return InvalidTransfer
      else do
         -- Lock all regions
         r0 <- lockRegion rm ReadOnly srcBuf srcReg
         rs <- forM steps $ \(TransferStep _ b r) -> lockRegion rm ReadWrite b r
         let res0 = (srcBuf,srcReg,r0)
             res1 = Prelude.map (\(TransferStep _ b r, res) -> (b,r,res)) (steps `zip` rs)
             result = Prelude.filter (\(_,_,r) -> r /= LockSuccess) (res0:res1)

         return $ if not (Prelude.null result) then LockError result else PrepareSuccess


prepareTransferIO :: TransferManager -> Transfer -> IO PrepareTransferResult
prepareTransferIO tm t = atomically $ prepareTransfer tm t

-- | Cancel a prepared transfer
--
-- Unlock every region involved
cancelTransfer :: TransferManager -> Transfer -> STM ()
cancelTransfer tm (Transfer srcBuf srcReg steps) = do
   let rm = regionManager tm

   -- Unlock all regions
   unlockRegion rm ReadOnly srcBuf srcReg
   forM_ steps $ \(TransferStep _ b r) -> unlockRegion rm ReadWrite b r


-- | Perform a transfer synchronously
performTransfer :: TransferManager -> Transfer -> IO [TransferResult]
performTransfer tm t@(Transfer srcBuf srcReg steps) = do
   evs <- forM steps (const $ newEvent)
   (_,_,errs) <- foldM f (srcBuf,srcReg,[]) (steps `zip` evs)
   atomically $ cancelTransfer tm t
   return (reverse errs)
   where
      f (b,r,errs) (TransferStep lnk tb tr, ev) = do
         if any (/= TransferSuccess) errs
            then return (b,r,errs)
            else do
               submitDirectTransfer tm (DirectTransfer lnk b r tb tr) ev
               err <- waitEvent ev
               return (tb,tr,err:errs)


submitDirectTransfer :: TransferManager -> DirectTransfer -> TransferEvent -> IO ()
submitDirectTransfer tm t@(DirectTransfer lnk _ _ _ _) ev = atomically $ writeTChan ch (t,ev)
   where
      ch = transferWorkers tm ! lnk
