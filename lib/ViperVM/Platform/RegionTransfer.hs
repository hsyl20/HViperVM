{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.RegionTransfer where

import ViperVM.Platform.Region
import qualified ViperVM.Backends.OpenCL.Link as CL
import ViperVM.Platform.Link
import ViperVM.Platform.Buffer
import ViperVM.Platform.TransferResult

data RegionTransferStep = RegionTransferStep Link Buffer Region
                    deriving (Eq,Ord)

data DirectRegionTransfer = DirectRegionTransfer Link Buffer Region Buffer Region
                            deriving (Eq,Ord,Show)

data RegionTransfer = RegionTransfer Buffer Region [RegionTransferStep]
                deriving (Eq,Ord)


-- | Check a transfer
checkRegionTransfer :: RegionTransfer -> Bool
checkRegionTransfer (RegionTransfer _ _ []) = True
checkRegionTransfer (RegionTransfer b1 r1 ((RegionTransferStep l b2 r2):xs)) = w
  where
      (lm1,lm2) = linkEndpoints l
      m1 = bufferMemory b1
      m2 = bufferMemory b2
      v = lm1 == m1 && lm2 == m2 && regionsWithSameShape r1 r2
      w = v && checkRegionTransfer (RegionTransfer b2 r2 xs)

                      
-- | Perform synchronous transfer of a region
transfer :: DirectRegionTransfer -> IO RegionTransferResult
transfer dt = case dt of

   -- Trick: Region2D (padding = 0), Region2D (padding = 0)
   DirectRegionTransfer link src (Region2D soff rows sz 0) dst (Region2D doff drows dsz 0) ->
      transfer (DirectRegionTransfer link src (Region1D soff (rows*sz)) dst (Region1D doff (drows*dsz)))

   -- OpenCl
   DirectRegionTransfer (CLLink link) src sreg dst dreg -> CL.linkTransfer link src sreg dst dreg
