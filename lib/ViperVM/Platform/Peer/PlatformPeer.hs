module ViperVM.Platform.Peer.PlatformPeer (
   PlatformPeer(..),initPlatform
) where

import qualified ViperVM.Backends.Host.Driver as Host
import qualified ViperVM.Backends.OpenCL.Driver as CL

import qualified ViperVM.Platform.Peer.MemoryPeer as Peer
import qualified ViperVM.Platform.Peer.LinkPeer as Peer
import qualified ViperVM.Platform.Peer.ProcPeer as Peer

import ViperVM.Platform.Configuration
import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.Platform.Proc

import Data.Traversable (forM)

data PlatformPeer = PlatformPeer {
  memories :: [Memory],
  links :: [Link],
  processors :: [Proc],
  configuration :: Configuration
}

initPlatform :: Configuration -> IO PlatformPeer
initPlatform config = do

   -- Initialize Host driver
   (hostMems,hostLinks) <- Host.initHost config

   let hostMemsPeer = fmap Peer.HostMemory hostMems
       hostLinksPeer = fmap Peer.HostLink hostLinks

   -- Initialize OpenCL driver
   (clMems,clLinks,clProcs) <- CL.initOpenCL config hostMems

   let clMemsPeer = fmap Peer.CLMemory clMems
       clLinksPeer = fmap Peer.CLLink clLinks
       clProcsPeer = fmap Peer.CLProc clProcs

   -- Wrap entities
   let memsPeer = hostMemsPeer ++ clMemsPeer
       linksPeer = hostLinksPeer ++ clLinksPeer
       procsPeer = clProcsPeer

   mems <- forM (memsPeer `zip` [0..]) (uncurry wrapMemory)
   lnks <- forM linksPeer (wrapLink mems)
   procs <- forM (procsPeer `zip` [0..]) (uncurry (wrapProc mems))

   return $ PlatformPeer mems lnks procs config
