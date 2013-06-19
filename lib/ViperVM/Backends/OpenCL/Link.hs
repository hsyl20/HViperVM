module ViperVM.Backends.OpenCL.Link (
   Link(..),
   linkEndpoints,
   linkTransfer
) where

import ViperVM.Platform.LinkCapabilities
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Event
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Platform.Memory
import ViperVM.Platform.Region
import ViperVM.Platform.Buffer
import ViperVM.Platform.TransferResult
import qualified ViperVM.Backends.Host.Buffer as Host
import qualified ViperVM.Backends.OpenCL.Buffer as CL

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow
import Text.Printf
import Foreign.Ptr
import Control.Monad (void,when)

data Link = Link {
   linkLibrary :: OpenCLLibrary,
   linkQueue   :: CLCommandQueue,
   linkSource  :: Memory,
   linkTarget  :: Memory,
   linkCapabilities :: Set LinkCapability
} deriving (Eq,Ord)

instance Show Link where
  show l = if linkSource l == linkTarget l
            then printf "OpenCL loopback link (%s)" (show (linkSource l))
            else printf "OpenCL link between %s and %s" (show (linkSource l)) (show (linkTarget l))

linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints = linkSource &&& linkTarget

linkTransfer :: Link -> Buffer -> Region -> Buffer -> Region -> IO RegionTransferResult
linkTransfer link srcBuf srcReg dstBuf dstReg = do

   let lib = linkLibrary link
       cq  = linkQueue link
   
   case (srcBuf,dstBuf) of
      -- Host -> CL
      (HostBuffer host,CLBuffer buf') -> do

         let ptr = Host.bufferPtr host
             buf = CL.bufferPeer buf'

         case (srcReg,dstReg) of
            -- Region 1D
            (Region1D soff sz, Region1D doff _) -> do
               let srcptr = plusPtr ptr (fromIntegral soff)
               e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
               void $ clWaitForEvents lib [e]
               void $ clReleaseEvent lib e
               return RegionTransferSuccess
            
            -- Region 2D
            (Region2D soff srowcount ssize spadding, Region2D doff drowcount dsize dpadding)
             | Set.member Transfer2D (linkCapabilities link) -> do
               let
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

             | otherwise -> error "2D Transfers not supported"

            _ -> return RegionTransferError

      -- CL -> Host
      (CLBuffer buf', HostBuffer host) -> do

         let ptr = Host.bufferPtr host
             buf = CL.bufferPeer buf'

         case (srcReg,dstReg) of
            -- Region 1D
            (Region1D soff sz, Region1D doff _) -> do
               let dstptr = plusPtr (Host.bufferPtr host) (fromIntegral doff)
               e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
               void $ clWaitForEvents lib [e]
               void $ clReleaseEvent lib e
               return RegionTransferSuccess

            -- Region 2D
            (Region2D doff drowcount dsize dpadding, Region2D soff srowcount ssize spadding)
             | Set.member Transfer2D (linkCapabilities link) -> do
               let reg = (ssize,srowcount,1)
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

             | otherwise -> error "2D Transfers not supported"

            _ -> return RegionTransferError

      -- CL -> CL
      (CLBuffer sBuf', CLBuffer dBuf') -> do

         let sBuf = CL.bufferPeer sBuf'
             dBuf = CL.bufferPeer dBuf'

         case (srcReg,dstReg) of
            -- Region 1D
            (Region1D soff sz, Region1D doff _) -> do
               e <- clEnqueueCopyBuffer lib cq sBuf dBuf soff doff sz []
               void $ clWaitForEvents lib [e]
               void $ clReleaseEvent lib e
               return RegionTransferSuccess

            -- Region 2D
            (Region2D soff srowcount ssize spadding, 
             Region2D doff drowcount dsize dpadding)
             | Set.member Transfer2D (linkCapabilities link) -> do
               let reg = (ssize,srowcount,1)
                   srowpitch = ssize + spadding
                   sslicepitch = 0
                   drowpitch = dsize + dpadding
                   dslicepitch = 0
                   sorigin = (soff, 0, 0)
                   dorigin = (doff, 0, 0)
               
               when (srowcount /= drowcount) (error "Regions do not have the same shape")

               e <- clEnqueueCopyBufferRect lib cq sBuf dBuf sorigin dorigin reg srowpitch sslicepitch drowpitch dslicepitch []
               void $ clWaitForEvents lib [e]
               void $ clReleaseEvent lib e
               return RegionTransferSuccess

             | otherwise -> error "2D Transfers not supported"

            _ -> return RegionTransferError

      _ -> return RegionTransferError
