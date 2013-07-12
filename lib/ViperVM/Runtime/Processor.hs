module ViperVM.Platform.Processor (
   Processor(..), procInfo, procMemories,
   procName, procVendor, procID, procSupports
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.ProcessorCapabilities
import qualified ViperVM.Backends.OpenCL.Processor as CL

import Data.Set (Set, member)
import Text.Printf

-- | A processing unit
data Processor = CLProcessor CL.Processor Memories

instance Eq Processor where
   (==) p1 p2 = procID p1 == procID p2

instance Ord Processor where
   compare p1 p2 = compare (procID p1) (procID p2)

instance Show Processor where
   show (CLProcessor p _) = show p

type Memories = Set Memory

-- | Apply the appropriate given function to the peer processor
applyPeerProc :: (CL.Processor -> a) -> Processor -> a
applyPeerProc f (CLProcessor p _) = f p

-- | Return processor name
procName :: Processor -> String
procName = applyPeerProc CL.procName

-- | Return processor vendor
procVendor :: Processor -> String
procVendor = applyPeerProc CL.procVendor

-- | Return processor ID
procID :: Processor -> String
procID = applyPeerProc CL.procID

-- | Get processor information string
procInfo :: Processor -> String
procInfo proc = printf "%s %s (%s)" (procID proc) (procName proc) (procVendor proc)

-- | Retrieve memories attached to a given processor
procMemories :: Processor -> Set Memory
procMemories (CLProcessor _ ms) = ms

-- | Retrieve processor capabilities
procCapabilities :: Processor -> Set ProcessorCapability
procCapabilities = applyPeerProc CL.procCapabilities

-- | Indicates if a processor supports a given capability
procSupports :: Processor -> ProcessorCapability -> Bool
procSupports p cap = cap `member` procCapabilities p
