module ViperVM.Platform.RegionTransferManager (
   RegionTransferManager, PrepareRegionTransferResult(..),
   createRegionTransferManager, prepareRegionTransfer, performRegionTransfer, cancelRegionTransfer,
   prepareRegionTransferIO
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.RegionTransfer
import ViperVM.Platform.Link
import ViperVM.Event

import Data.Map
import Data.Traversable (forM)
import Data.Foldable(forM_)

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (foldM)

type RegionTransferEvent = Event RegionTransferResult

data RegionTransferManager = RegionTransferManager {
      regionManager :: RegionLockManager,
      transferWorkers :: Map Link (TChan (DirectRegionTransfer, RegionTransferEvent))
   }

data PrepareRegionTransferResult = PrepareSuccess | 
                                   LockError [(Buffer,Region,RegionLockResult)] |
                                   InvalidRegionTransfer
                                   deriving (Eq,Ord)

-- | Initialize a new transfer manager
createRegionTransferManager :: RegionLockManager -> IO RegionTransferManager
createRegionTransferManager rm = do
   let pf = getPlatform rm

   -- Create transfer threads
   threads <- forM (links pf) $ \link -> do
      trs <- atomically newTChan
      _ <- forkOS (transferThread link trs)
      return (link, trs)
      
   return $ RegionTransferManager rm (fromList threads)


-- | Thread that perform transfers on a given link
transferThread :: Link -> TChan (DirectRegionTransfer, RegionTransferEvent) -> IO ()
transferThread lnk ch = do
   (tr,ev) <- atomically $ readTChan ch

   err <- transfer tr
   setEvent ev err

   transferThread lnk ch
   

-- | Prepare a transfer
--
-- Lock every region involved
prepareRegionTransfer :: RegionTransferManager -> RegionTransfer -> STM PrepareRegionTransferResult
prepareRegionTransfer tm t@(RegionTransfer srcBuf srcReg steps) = do
   let rm = regionManager tm

   if not (checkRegionTransfer t)
      then return InvalidRegionTransfer
      else do
         -- Lock all regions
         r0 <- lockRegion rm ReadOnly srcBuf srcReg
         rs <- forM steps $ \(RegionTransferStep _ b r) -> lockRegion rm ReadWrite b r
         let res0 = (srcBuf,srcReg,r0)
             res1 = Prelude.map (\(RegionTransferStep _ b r, res) -> (b,r,res)) (steps `zip` rs)
             result = Prelude.filter (\(_,_,r) -> r /= LockSuccess) (res0:res1)

         return $ if not (Prelude.null result) then LockError result else PrepareSuccess


prepareRegionTransferIO :: RegionTransferManager -> RegionTransfer -> IO PrepareRegionTransferResult
prepareRegionTransferIO tm t = atomically $ prepareRegionTransfer tm t

-- | Cancel a prepared transfer
--
-- Unlock every region involved
cancelRegionTransfer :: RegionTransferManager -> RegionTransfer -> STM ()
cancelRegionTransfer tm (RegionTransfer srcBuf srcReg steps) = do
   let rm = regionManager tm

   -- Unlock all regions
   unlockRegion rm ReadOnly srcBuf srcReg
   forM_ steps $ \(RegionTransferStep _ b r) -> unlockRegion rm ReadWrite b r


-- | Perform a transfer synchronously
performRegionTransfer :: RegionTransferManager -> RegionTransfer -> IO [RegionTransferResult]
performRegionTransfer tm t@(RegionTransfer srcBuf srcReg steps) = do
   evs <- forM steps (const $ newEvent)
   (_,_,errs) <- foldM f (srcBuf,srcReg,[]) (steps `zip` evs)
   atomically $ cancelRegionTransfer tm t
   return (reverse errs)
   where
      f (b,r,errs) (RegionTransferStep lnk tb tr, ev) = do
         if any (/= RegionTransferSuccess) errs
            then return (b,r,errs)
            else do
               submitDirectRegionTransfer tm (DirectRegionTransfer lnk b r tb tr) ev
               err <- waitEvent ev
               return (tb,tr,err:errs)


submitDirectRegionTransfer :: RegionTransferManager -> DirectRegionTransfer -> RegionTransferEvent -> IO ()
submitDirectRegionTransfer tm t@(DirectRegionTransfer lnk _ _ _ _) ev = atomically $ writeTChan ch (t,ev)
   where
      ch = transferWorkers tm ! lnk
