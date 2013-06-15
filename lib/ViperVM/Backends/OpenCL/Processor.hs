module ViperVM.Backends.OpenCL.Processor (
      Processor(..),
      procName, procVendor, procMemories,
      procSupports
   ) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query
import ViperVM.Backends.OpenCL.Memory
import ViperVM.Platform.ProcessorCapabilities

import System.IO.Unsafe

data Processor = Processor {
   procLibrary :: OpenCLLibrary,
   procContext :: CLContext,
   procQueue   :: CLCommandQueue,
   procDevice  :: CLDeviceID,
   procID      :: String
}

instance Eq Processor where
  (==) p1 p2 = procID p1 == procID p2

instance Ord Processor where
  compare p1 p2 = compare (procID p1) (procID p2)

instance Show Processor where
  show p = "{" ++ procID p ++ "}"

procName :: Processor -> IO String
procName p = clGetDeviceName (procLibrary p) (procDevice p)

procVendor :: Processor -> IO String
procVendor p = clGetDeviceVendor (procLibrary p) (procDevice p)

procMemories :: Processor -> [Memory]
procMemories p = [Memory (procLibrary p) (procContext p) (procDevice p)]

procSupports :: Processor -> ProcessorCapability -> Bool
procSupports p DoubleFloatingPoint = 
   not . null . unsafePerformIO $ clGetDeviceDoubleFPConfig (procLibrary p) (procDevice p)


