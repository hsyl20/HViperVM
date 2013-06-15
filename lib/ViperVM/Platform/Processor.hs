module ViperVM.Platform.Processor (
   Processor(..), procInfo, processorMemories,
   procName, procVendor, procID, procSupports
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.ProcessorCapabilities
import qualified ViperVM.Backends.OpenCL.Processor as CL

import Control.Applicative ( (<$>), (<*>) )
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
procName :: Processor -> IO String
procName (CLProcessor p) = CL.procName p

-- | Return processor vendor
procVendor :: Processor -> IO String
procVendor (CLProcessor p) = CL.procVendor p

-- | Return processor ID
procID :: Processor -> String
procID (CLProcessor p) = CL.procID p

-- | Get processor information string
procInfo :: Processor -> IO String
procInfo proc = printf "%s %s (%s)" (procID proc) <$> procName proc <*> procVendor proc

-- | Retrieve memories attached to a given processor
processorMemories :: Processor -> [Memory]
processorMemories (CLProcessor p) = fmap CLMemory (CL.procMemories p)

procSupports :: Processor -> ProcessorCapability -> Bool
procSupports (CLProcessor p) = CL.procSupports p
