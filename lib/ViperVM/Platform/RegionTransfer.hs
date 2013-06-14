{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.RegionTransfer where

import ViperVM.Platform.Region
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Link
import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import Control.Monad (void)
import Foreign.Ptr

data RegionTransferResult =
     RegionTransferSuccess 
   | RegionTransferError DirectRegionTransfer
   deriving (Eq,Ord,Show)

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
      m1 = getBufferMemory b1
      m2 = getBufferMemory b2
      v = lm1 == m1 && lm2 == m2 && regionsWithSameShape r1 r2
      w = v && checkRegionTransfer (RegionTransfer b2 r2 xs)

                      
-- | Perform synchronous transfer of a region
transfer :: DirectRegionTransfer -> IO RegionTransferResult
transfer dt = case dt of

   -- Region2D (padding = 0), Region2D (padding = 0)
   DirectRegionTransfer link src (Region2D soff rows sz 0) dst (Region2D doff drows dsz 0) ->
      transfer (DirectRegionTransfer link src (Region1D soff (rows*sz)) dst (Region1D doff (drows*dsz)))

   -- Host --> CL, Region1D, Region1D
   DirectRegionTransfer (CLLink lib cq HostMemory (CLMemory {}) _) (HostBuffer _ ptr) (Region1D soff sz) (CLBuffer _ _ buf _) (Region1D doff _) -> do
      let srcptr = plusPtr ptr (fromIntegral soff)
      e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
      void $ clWaitForEvents lib [e]
      void $ clReleaseEvent lib e
      return RegionTransferSuccess

   -- CL --> Host, Region1D, Region1D
   DirectRegionTransfer (CLLink lib cq (CLMemory {}) HostMemory _) (CLBuffer _ _ buf _) (Region1D soff sz) (HostBuffer _ ptr) (Region1D doff _) -> do
      let dstptr = plusPtr ptr (fromIntegral doff)
      e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
      void $ clWaitForEvents lib [e]
      void $ clReleaseEvent lib e
      return RegionTransferSuccess

   _ -> return (RegionTransferError dt)
