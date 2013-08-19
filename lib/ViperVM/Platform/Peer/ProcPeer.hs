-- | Backend specific glue for processors
module ViperVM.Platform.Peer.ProcPeer (
  ProcPeer(..), procPeerApply,
  procName, procVendor, procCapabilities,
  procMemories
) where

import ViperVM.Platform.ProcessorCapabilities
import qualified ViperVM.Backends.OpenCL.Processor as CL

import qualified ViperVM.Platform.Peer.MemoryPeer as Peer

import Data.Set (Set)
import Control.Applicative ( (<$>) )

data ProcPeer = CLProc CL.Processor
                deriving (Eq,Ord,Show)

-- | Apply the appropriate given function to the peer processor
procPeerApply :: (CL.Processor -> a) -> ProcPeer -> a
procPeerApply fCL p = case p of
   CLProc peer -> fCL peer

-- | Return processor name
procName :: ProcPeer -> String
procName = procPeerApply CL.procName

-- | Return processor vendor
procVendor :: ProcPeer -> String
procVendor = procPeerApply CL.procVendor

-- | Retrieve processor capabilities
procCapabilities :: ProcPeer -> Set ProcessorCapability
procCapabilities = procPeerApply CL.procCapabilities

-- | Retrieve memories associated to the proc
procMemories :: ProcPeer -> IO [Peer.MemoryPeer]
procMemories = procPeerApply 
   (\p -> fmap Peer.CLMemory <$> CL.procMemories p)
