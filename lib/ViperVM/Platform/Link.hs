module ViperVM.Platform.Link (
   Link(..), wrapLink,
   linkInfo, linkEndpoints, linksBetween,
   linkCapabilities, linkTransfer
) where

import qualified ViperVM.Platform.Peer.LinkPeer as Peer
import ViperVM.Platform.Memory
import ViperVM.Platform.LinkCapabilities
import ViperVM.Common.Region

import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow

-- | A link between two memories
data Link = Link {
   linkPeer :: Peer.LinkPeer,
   linkSource :: Memory,
   linkTarget :: Memory
}

instance Show Link where
  show l = "Link " ++ show (linkId l)

-- | Link identifier
linkId :: Link -> (Int,Int)
linkId l = (memoryId (linkSource l), memoryId (linkTarget l))

-- | Wrap a peer link
wrapLink :: [Memory] -> Peer.LinkPeer -> IO Link
wrapLink mems l = return (Link l src dst)
   where
      srcPeer = Peer.linkSource l
      dstPeer = Peer.linkTarget l
      mems' = fmap (\m -> (memoryPeer m, m)) mems
      Just src = lookup srcPeer mems'
      Just dst = lookup dstPeer mems'

-- | Retrieve link information string
linkInfo :: Link -> String
linkInfo l = if linkSource l == linkTarget l 
      then printf "Loopback link in %s" name1
      else printf "Link between %s and %s" name1 name2
   where
      name1 = memoryName (linkSource l)
      name2 = memoryName (linkTarget l)
      
-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints = linkSource &&& linkTarget

-- | Get links between a source and a destination from a list of links
linksBetween :: Memory -> Memory -> [Link] -> [Link]
linksBetween m1 m2 = filter ((==) (m1,m2) . linkEndpoints)

-- | Retrieve link capabilities
linkCapabilities :: Link -> Set LinkCapability
linkCapabilities = Peer.linkCapabilities . linkPeer

-- | Perform a synchronous transfer on a link
linkTransfer :: Link -> Buffer -> Region -> Buffer -> Region -> IO ()
linkTransfer link src sreg dst dreg = do
   let 
      linkP = linkPeer link
      srcP = bufferPeer src
      dstP = bufferPeer dst

   case (sreg,dreg) of
      
      (Region1D {}, Region1D {}) -> Peer.link1DTransfer linkP srcP sreg dstP dreg

      (Region2D {}, Region2D {})
         | Set.member Transfer2D (linkCapabilities link) -> Peer.link2DTransfer linkP srcP sreg dstP dreg
         | otherwise -> error "2D transfers not supported by this link" -- TODO: fallback to several 1D transfers

      _ -> error "Invalid transfer"

