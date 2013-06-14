{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.RegionTransfer where

import ViperVM.Platform.Region
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Link
import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import Control.Monad (void,when)
import Foreign.Ptr
import qualified Data.Set as Set

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

   -- Host --> CL, Region2D, Region2D
   DirectRegionTransfer lk@(CLLink lib cq HostMemory (CLMemory {}) _) (HostBuffer _ ptr) r1@(Region2D {}) (CLBuffer _ _ buf _) r2@(Region2D {}) 
      | Set.member Transfer2D (linkCapabilities lk) -> do
      let (Region2D soff srowcount ssize spadding) = r1
          (Region2D doff drowcount dsize dpadding) = r2
          reg = (ssize,srowcount,1)
          srowpitch = ssize + spadding
          sslicepitch = 0
          drowpitch = dsize + dpadding
          dslicepitch = 0
          sorigin = (soff, 0, 0)
          dorigin = (doff, 0, 0)
      
      when (srowcount /= drowcount) (error "Regions do not have the same shape")

      e <- clEnqueueWriteBufferRect lib cq buf True dorigin sorigin reg drowpitch dslicepitch srowpitch sslicepitch ptr []
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

   -- CL --> Host, Region2D, Region2D
   DirectRegionTransfer lk@(CLLink lib cq (CLMemory {}) HostMemory _) (CLBuffer _ _ buf _) r2@(Region2D {}) (HostBuffer _ ptr) r1@(Region2D {}) 
      | Set.member Transfer2D (linkCapabilities lk) -> do
      let (Region2D soff srowcount ssize spadding) = r1
          (Region2D doff drowcount dsize dpadding) = r2
          reg = (ssize,srowcount,1)
          srowpitch = ssize + spadding
          sslicepitch = 0
          drowpitch = dsize + dpadding
          dslicepitch = 0
          sorigin = (soff, 0, 0)
          dorigin = (doff, 0, 0)
      
      when (srowcount /= drowcount) (error "Regions do not have the same shape")

      e <- clEnqueueReadBufferRect lib cq buf True dorigin sorigin reg drowpitch dslicepitch srowpitch sslicepitch ptr []
      void $ clWaitForEvents lib [e]
      void $ clReleaseEvent lib e
      return RegionTransferSuccess
   _ -> return (RegionTransferError dt)
