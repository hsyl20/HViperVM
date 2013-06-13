module ViperVM.Platform.Processor (
   Processor(..), procInfo, processorMemories,
   procCLDevice, procId, procCLLib, procCLContext
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory

import Control.Applicative ( (<$>), (<*>) )
import Text.Printf

-- | A processing unit
data Processor = HostProcessor
               | CLProcessor OpenCLLibrary CLContext CLCommandQueue CLDeviceID ProcID

type ProcID = String

instance Eq Processor where
  (==) HostProcessor HostProcessor = True
  (==) (CLProcessor _ _ _ _ id1) (CLProcessor _ _ _ _ id2) = id1 == id2
  (==) _ _ = False

instance Ord Processor where
  compare HostProcessor HostProcessor = EQ
  compare HostProcessor _ = GT
  compare _ HostProcessor = LT
  compare (CLProcessor _ _ _ _ id1) (CLProcessor _ _ _ _ id2) = compare id1 id2

instance Show Processor where
  show = procId

-- | Return processor unique identifier
procId :: Processor -> String
procId (CLProcessor _ _ _ _ pid) = printf "{%s}" pid
procId HostProcessor = "{Host}"

-- | Return processor name
procName :: Processor -> IO String
procName (CLProcessor lib _ _ dev _) = clGetDeviceName lib dev
procName HostProcessor = return "Host processor"

-- | Return processor vendor
procVendor :: Processor -> IO String
procVendor (CLProcessor lib _ _ dev _) = clGetDeviceVendor lib dev
procVendor HostProcessor = return ""

-- | Return OpenCL device associated to the processor
procCLDevice :: Processor -> CLDeviceID
procCLDevice (CLProcessor _ _ _ dev _) = dev
procCLDevice _ = error "Not a valid OpenCL device"

-- | Return associated OpenCL library
procCLLib :: Processor -> OpenCLLibrary
procCLLib (CLProcessor lib _ _ _ _) = lib
procCLLib _ = error "Not a valid OpenCL device"

-- | Return associated OpenCL context
procCLContext :: Processor -> CLContext
procCLContext (CLProcessor _ ctx _ _ _) = ctx
procCLContext _ = error "Not a valid OpenCL device"

-- | Get processor information string
procInfo :: Processor -> IO String
procInfo proc = printf "%s %s (%s)" (procId proc) <$> procName proc <*> procVendor proc

-- | Retrieve memories attached to a given processor
processorMemories :: Processor -> [Memory]
processorMemories (CLProcessor lib ctx _ dev _) = [CLMemory lib ctx dev]
processorMemories HostProcessor = [HostMemory]
