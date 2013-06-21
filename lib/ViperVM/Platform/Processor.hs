module ViperVM.Platform.Processor (
   Processor(..), procInfo, procMemories,
   procName, procVendor, procID, procSupports
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.ProcessorCapabilities
import qualified ViperVM.Backends.OpenCL.Processor as CL

import Text.Printf

-- | A processing unit
data Processor = CLProcessor CL.Processor

instance Eq Processor where
   (==) p1 p2 = procID p1 == procID p2

instance Ord Processor where
   compare p1 p2 = compare (procID p1) (procID p2)

instance Show Processor where
   show (CLProcessor p) = show p

-- | Return processor name
procName :: Processor -> String
procName (CLProcessor p) = CL.procName p

-- | Return processor vendor
procVendor :: Processor -> String
procVendor (CLProcessor p) = CL.procVendor p

-- | Return processor ID
procID :: Processor -> String
procID (CLProcessor p) = CL.procID p

-- | Get processor information string
procInfo :: Processor -> String
procInfo proc = printf "%s %s (%s)" (procID proc) (procName proc) (procVendor proc)

-- | Retrieve memories attached to a given processor
procMemories :: Processor -> [Memory]
procMemories (CLProcessor p) = fmap CLMemory (CL.procMemories p)

-- | Retrieve processor capabilities
procCapabilities :: Processor -> [ProcessorCapability]
procCapabilities (CLProcessor p) = CL.procCapabilities p

-- | Indicates if a processor supports a given capability
procSupports :: Processor -> ProcessorCapability -> Bool
procSupports p cap = cap `elem` procCapabilities p
