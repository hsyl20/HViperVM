module ViperVM.Backends.Host.Link (
   Link, initLink, linkSource, linkTarget, linkCapabilities,
   linkTransfer
) where

import ViperVM.Platform.Region
import ViperVM.Platform.LinkCapabilities
import qualified ViperVM.Backends.Host.Buffer as Host
import qualified ViperVM.Backends.Host.Memory as Host
import qualified ViperVM.Platform.Peer.MemoryPeer as Peer

import Data.Foldable (forM_)
import Foreign.Ptr
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf
import Data.Int
import Foreign.Marshal.Utils

data Link = Link (Set LinkCapability) Host.Memory
            deriving (Eq,Ord)

instance Show Link where
  show _ = printf "Host loopback link"

-- | Initialize host link
initLink :: Host.Memory -> Link
initLink m = Link (Set.fromList [Transfer2D]) m

linkCapabilities :: Link -> Set LinkCapability
linkCapabilities (Link caps _) = caps

linkSource :: Link -> Peer.MemoryPeer
linkSource (Link _ m) = Peer.HostMemory m

linkTarget :: Link -> Peer.MemoryPeer
linkTarget (Link _ m) = Peer.HostMemory m

linkTransfer :: Link -> Host.Buffer -> Region -> Host.Buffer -> Region -> IO()
linkTransfer _ src srcReg dst dstReg = do
   
   let 
      sptr = castPtr (Host.bufferPtr src) :: Ptr Int8
      dptr = castPtr (Host.bufferPtr dst) :: Ptr Int8

   case (srcReg, dstReg) of

      -- Region 1D
      (Region1D soff sz, Region1D doff _) -> do
         let
            sptr' = sptr `plusPtr` (fromIntegral soff)
            dptr' = dptr `plusPtr` (fromIntegral doff)
         
         copyBytes dptr' sptr' (fromIntegral sz)


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

      _ -> error "Invalid transfer"
