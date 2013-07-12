module ViperVM.Platform.Proc (
   Proc(..), wrapProc, procInfo, procCapabilities,
   procName, procVendor, procSupports
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.ProcessorCapabilities

import qualified ViperVM.Platform.Peer.ProcPeer as Peer

import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)
import Text.Printf

-- | A processing unit
data Proc = Proc {
   procId :: Int,
   procPeer :: Peer.ProcPeer,
   procMemories :: Set Memory
}

instance Eq Proc where
   (==) p1 p2 = procId p1 == procId p2

instance Ord Proc where
   compare p1 p2 = compare (procId p1) (procId p2)

instance Show Proc where
   show p = "Proc " ++ show (procId p)

-- | Wrap a peer proc
wrapProc :: [Memory] -> Peer.ProcPeer -> Int -> IO Proc
wrapProc mems peer pId = do
      peerMems <- Peer.procMemories peer
      let 
         mems' = fmap (\m -> (memoryPeer m, m)) mems
         ms = Set.fromList $ concatMap (\m -> maybeToList (lookup m mems')) peerMems

      return (Proc pId peer ms)

-- | Return processor name
procName :: Proc -> String
procName = Peer.procName . procPeer

-- | Return processor vendor
procVendor :: Proc -> String
procVendor = Peer.procVendor . procPeer

-- | Retrieve processor capabilities
procCapabilities :: Proc -> Set ProcessorCapability
procCapabilities = Peer.procCapabilities . procPeer

-- | Get processor information string
procInfo :: Proc -> String
procInfo proc = printf "[Proc %d] %s (%s)" (procId proc) (procName proc) (procVendor proc)

-- | Indicates if a processor supports a given capability
procSupports :: Proc -> ProcessorCapability -> Bool
procSupports p cap = cap `member` procCapabilities p
