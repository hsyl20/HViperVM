module ViperVM.Platform.Processor (
   Processor(..), procInfo, processorMemories,
   procCLDevice
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory
import System.IO.Unsafe

-- | A processing unit
data Processor = HostProcessor
               | CLProcessor OpenCLLibrary CLContext CLCommandQueue CLDeviceID

instance Eq Processor where
  (==) HostProcessor HostProcessor = True
  (==) (CLProcessor lib1 _ _ id1) (CLProcessor lib2 _ _ id2) = lib1 == lib2 && id1 == id2
  (==) _ _ = False

instance Ord Processor where
  compare HostProcessor HostProcessor = EQ
  compare HostProcessor _ = GT
  compare _ HostProcessor = LT
  compare (CLProcessor _ _ _ id1) (CLProcessor _ _ _ id2) = compare id1 id2

instance Show Processor where
  show p = unsafePerformIO $ procInfo p


-- | Return OpenCL device associated to the processor
procCLDevice :: Processor -> CLDeviceID
procCLDevice (CLProcessor _ _ _ dev) = dev
procCLDevice _ = error "Not a valid OpenCL device"

-- | Get processor information string
procInfo :: Processor -> IO String
procInfo (CLProcessor lib _ _ dev) = do
  name <- clGetDeviceName lib dev
  vendor <- clGetDeviceVendor lib dev
  return $ "[OpenCL] " ++ name ++ " (" ++ vendor ++ ")"

procInfo HostProcessor = return "[Host]"

-- | Retrieve memories attached to a given processor
processorMemories :: Processor -> [Memory]
processorMemories (CLProcessor lib ctx _ dev) = [CLMemory lib ctx dev]
processorMemories HostProcessor = [HostMemory]
