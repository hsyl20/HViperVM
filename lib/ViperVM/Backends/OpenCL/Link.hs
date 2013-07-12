module ViperVM.Backends.OpenCL.Link (
   Link(..), linkLibrary, linkCapabilities, linkQueue,
   linkSource, linkTarget,
   hostToCL1D, hostToCL2D,
   clToHost1D, clToHost2D,
   clToCL1D, clToCL2D
) where

import ViperVM.Platform.LinkCapabilities
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Event
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Common.Region

import qualified ViperVM.Platform.Peer.MemoryPeer as Peer

import qualified ViperVM.Backends.Host.Buffer as Host
import qualified ViperVM.Backends.Host.Memory as Host
import qualified ViperVM.Backends.OpenCL.Buffer as CL
import qualified ViperVM.Backends.OpenCL.Memory as CL

import Data.Set (Set)
import Text.Printf
import Foreign.Ptr
import Control.Monad (void)

data Link = 
     HostCLLink OpenCLLibrary CLCommandQueue (Set LinkCapability) Host.Memory CL.Memory
   | CLHostLink OpenCLLibrary CLCommandQueue (Set LinkCapability) CL.Memory Host.Memory
   | CLLoopBackLink OpenCLLibrary CLCommandQueue (Set LinkCapability) CL.Memory
   deriving (Eq,Ord)

linkLibrary :: Link -> OpenCLLibrary
linkLibrary (HostCLLink lib _ _ _ _) = lib
linkLibrary (CLHostLink lib _ _ _ _) = lib
linkLibrary (CLLoopBackLink lib _ _ _) = lib

linkQueue :: Link -> CLCommandQueue
linkQueue (HostCLLink _ cq _ _ _) = cq
linkQueue (CLHostLink _ cq _ _ _) = cq
linkQueue (CLLoopBackLink _ cq _ _) = cq

linkCapabilities :: Link -> Set LinkCapability
linkCapabilities (HostCLLink _ _ caps _ _) = caps
linkCapabilities (CLHostLink _ _ caps _ _) = caps
linkCapabilities (CLLoopBackLink _ _ caps _) = caps

linkSource :: Link -> Peer.MemoryPeer
linkSource (HostCLLink _ _ _ m _) = Peer.HostMemory m
linkSource (CLHostLink _ _ _ m _) = Peer.CLMemory m
linkSource (CLLoopBackLink _ _ _ m) = Peer.CLMemory m

linkTarget :: Link -> Peer.MemoryPeer
linkTarget (HostCLLink _ _ _ _ m) = Peer.CLMemory m
linkTarget (CLHostLink _ _ _ _ m) = Peer.HostMemory m
linkTarget (CLLoopBackLink _ _ _ m) = Peer.CLMemory m

instance Show Link where
  show (CLLoopBackLink _ _ _ m) = printf "OpenCL loopback link (%s)" (show m)
  show (HostCLLink _ _ _ src dst) = printf "OpenCL link between %s and %s" (show src) (show dst)
  show (CLHostLink _ _ _ src dst) = printf "OpenCL link between %s and %s" (show src) (show dst)

waitTransfer :: OpenCLLibrary -> IO CLEvent -> IO ()
waitTransfer lib f = do
   e <- f
   void $ clWaitForEvents lib [e]
   void $ clReleaseEvent lib e


hostToCL1D :: Link -> Host.Buffer -> Region -> CL.Buffer -> Region -> IO ()
hostToCL1D link src sreg dst dreg = waitTransfer lib (clEnqueueWriteBuffer lib cq buf True doff ssz srcptr [])
   where 
      ptr = Host.bufferPtr src
      buf = CL.bufferPeer dst
      srcptr = plusPtr ptr (fromIntegral soff)
      Region1D soff ssz = sreg
      Region1D doff _ = dreg
      lib = linkLibrary link
      cq  = linkQueue link

clToHost1D :: Link -> CL.Buffer -> Region -> Host.Buffer -> Region -> IO ()
clToHost1D link src sreg dst dreg = waitTransfer lib (clEnqueueReadBuffer lib cq buf True soff ssz dstptr [])
   where 
      ptr = Host.bufferPtr dst
      buf = CL.bufferPeer src
      dstptr = plusPtr ptr (fromIntegral doff)
      Region1D soff ssz = sreg
      Region1D doff _ = dreg
      lib = linkLibrary link
      cq  = linkQueue link

clToCL1D :: Link -> CL.Buffer -> Region -> CL.Buffer -> Region -> IO ()
clToCL1D link src sreg dst dreg = waitTransfer lib (clEnqueueCopyBuffer lib cq sBuf dBuf soff doff ssz [])
   where
      sBuf = CL.bufferPeer src
      dBuf = CL.bufferPeer dst
      Region1D soff ssz = sreg
      Region1D doff _ = dreg
      lib = linkLibrary link
      cq  = linkQueue link


hostToCL2D :: Link -> Host.Buffer -> Region -> CL.Buffer -> Region -> IO ()
hostToCL2D link src sreg dst dreg = waitTransfer lib (clEnqueueWriteBufferRect lib cq buf True dorigin sorigin reg drowpitch dslicepitch srowpitch sslicepitch ptr [])
   where
      ptr = Host.bufferPtr src
      buf = CL.bufferPeer dst
      reg = (ssize,srowcount,1)
      srowpitch = ssize + spadding
      sslicepitch = 0
      drowpitch = dsize + dpadding
      dslicepitch = 0
      sorigin = (soff, 0, 0)
      dorigin = (doff, 0, 0)
      Region2D soff srowcount ssize spadding = sreg
      Region2D doff _ dsize dpadding = dreg
      lib = linkLibrary link
      cq  = linkQueue link

clToHost2D :: Link -> CL.Buffer -> Region -> Host.Buffer -> Region -> IO ()
clToHost2D link src sreg dst dreg = waitTransfer lib (clEnqueueReadBufferRect lib cq buf True dorigin sorigin reg drowpitch dslicepitch srowpitch sslicepitch ptr [])
   where
      ptr = Host.bufferPtr dst
      buf = CL.bufferPeer src
      reg = (ssize,srowcount,1)
      srowpitch = ssize + spadding
      sslicepitch = 0
      drowpitch = dsize + dpadding
      dslicepitch = 0
      sorigin = (soff, 0, 0)
      dorigin = (doff, 0, 0)
      Region2D soff srowcount ssize spadding = sreg
      Region2D doff _ dsize dpadding = dreg
      lib = linkLibrary link
      cq  = linkQueue link


clToCL2D :: Link -> CL.Buffer -> Region -> CL.Buffer -> Region -> IO ()
clToCL2D link src sreg dst dreg = waitTransfer lib (clEnqueueCopyBufferRect lib cq sBuf dBuf sorigin dorigin reg srowpitch sslicepitch drowpitch dslicepitch [])
   where
      sBuf = CL.bufferPeer src
      dBuf = CL.bufferPeer dst
      reg = (ssize,srowcount,1)
      srowpitch = ssize + spadding
      sslicepitch = 0
      drowpitch = dsize + dpadding
      dslicepitch = 0
      sorigin = (soff, 0, 0)
      dorigin = (doff, 0, 0)
      Region2D soff srowcount ssize spadding = sreg
      Region2D doff _ dsize dpadding = dreg
      lib = linkLibrary link
      cq  = linkQueue link
               
