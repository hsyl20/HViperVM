module ViperVM.Backends.Host.Link (
   Link, initLink, linkSource, linkTarget, linkCapabilities,
   linkTransfer
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.TransferResult
import ViperVM.Platform.LinkCapabilities
import ViperVM.Platform.Buffer
import qualified ViperVM.Backends.Host.Buffer as Host

import Data.Foldable (forM_)
import Foreign.Ptr
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Int
import Foreign.Marshal.Utils

data Link = Link {
   linkSource :: Memory,
   linkTarget :: Memory,
   linkCapabilities :: Set LinkCapability
} deriving (Eq,Ord)

instance Show Link where
  show _ = printf "Host loopback link"

-- | Initialize host link
initLink :: Memory -> Link
initLink m = Link m m (Set.fromList [Transfer2D])

linkTransfer :: Link -> Buffer -> Region -> Buffer -> Region -> IO RegionTransferResult
linkTransfer _ (HostBuffer srcBuf) srcReg (HostBuffer dstBuf) dstReg = do
   
   let 
      sptr = castPtr (Host.bufferPtr srcBuf) :: Ptr Int8
      dptr = castPtr (Host.bufferPtr dstBuf) :: Ptr Int8

   case (srcReg, dstReg) of

      -- Region 1D
      (Region1D soff sz, Region1D doff _) -> do
         let
            sptr' = sptr `plusPtr` (fromIntegral soff)
            dptr' = dptr `plusPtr` (fromIntegral doff)
         
         copyBytes dptr' sptr' (fromIntegral sz)
         return RegionTransferSuccess


      -- Region 2D
      (Region2D soff srowcount ssize spadding, 
       Region2D doff _ dsize dpadding) -> do
         let
            sptr' = sptr `plusPtr` (fromIntegral soff)
            dptr' = dptr `plusPtr` (fromIntegral doff)
            spad = ssize + spadding
            dpad = dsize + dpadding
         
         forM_ [0..srowcount-1] $ \i -> do
            let
               sptr'' = sptr' `plusPtr` (fromIntegral $ i*spad)
               dptr'' = dptr' `plusPtr` (fromIntegral $ i*dpad)
            copyBytes dptr'' sptr'' (fromIntegral ssize)

         return RegionTransferSuccess

      _ -> return RegionTransferError

linkTransfer _ _ _ _ _ = return RegionTransferError
