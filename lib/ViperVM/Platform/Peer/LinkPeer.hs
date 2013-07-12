module ViperVM.Platform.Peer.LinkPeer (
   LinkPeer(..), link1DTransfer, link2DTransfer,
   linkCapabilities, linkSource, linkTarget
) where

import qualified ViperVM.Backends.OpenCL.Link as CL
import qualified ViperVM.Backends.Host.Link as Host

import ViperVM.Platform.Peer.MemoryPeer
import ViperVM.Platform.Region
import ViperVM.Platform.LinkCapabilities

import Control.Monad (when)
import Data.Set (Set)

data LinkPeer = HostLink Host.Link
              | CLLink CL.Link
              deriving (Eq,Ord,Show)

linkSource :: LinkPeer -> MemoryPeer
linkSource (HostLink l) = Host.linkSource l
linkSource (CLLink l) = CL.linkSource l

linkTarget :: LinkPeer -> MemoryPeer
linkTarget (HostLink l) = Host.linkTarget l
linkTarget (CLLink l) = CL.linkTarget l

-- | Retrieve link capabilities
linkCapabilities :: LinkPeer -> Set LinkCapability
linkCapabilities (CLLink l) = CL.linkCapabilities l
linkCapabilities (HostLink l) = Host.linkCapabilities l

-- | 1D Transfer
link1DTransfer :: LinkPeer -> BufferPeer -> Region -> BufferPeer -> Region -> IO ()
link1DTransfer link src sreg dst dreg = do

   let
      Region1D _ ssz = sreg
      Region1D _ dsz = dreg

   when (ssz /= dsz) (error "Invalid region size")

   case (link,src,dst) of
      (CLLink l, HostBuffer b1, CLBuffer b2) -> CL.hostToCL1D l b1 sreg b2 dreg
      (CLLink l, CLBuffer b1, HostBuffer b2) -> CL.clToHost1D l b1 sreg b2 dreg
      (CLLink l, CLBuffer b1, CLBuffer b2)   -> CL.clToCL1D l b1 sreg b2 dreg
      _ -> error "Invalid transfer"



-- | 2D Transfer
link2DTransfer :: LinkPeer -> BufferPeer -> Region -> BufferPeer -> Region -> IO ()
link2DTransfer link src sreg dst dreg = do

   let
      Region2D _ srowcount ssize _ = sreg
      Region2D _ drowcount dsize _ = dreg

   when (ssize /= dsize || srowcount /= drowcount) 
      (error "Invalid region size")

   case (link,src,dst) of
      (CLLink l, HostBuffer b1, CLBuffer b2) -> CL.hostToCL2D l b1 sreg b2 dreg
      (CLLink l, CLBuffer b1, HostBuffer b2) -> CL.clToHost2D l b1 sreg b2 dreg

      (CLLink l, CLBuffer b1, CLBuffer b2)   -> CL.clToCL2D l b1 sreg b2 dreg

      _ -> error "Invalid transfer"
