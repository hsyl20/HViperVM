module ViperVM.Backends.OpenCL.Processor (
   Processor, initProc,
   procLibrary, procContext, procDevice,
   procQueue, procID, procName, procVendor,
   procCapabilities, procMemories,
   programCompile
) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Memory
import ViperVM.Platform.ProcessorCapabilities
import ViperVM.Platform.Compilation

import Data.Set (Set,fromList)
import Text.Printf
import Control.Applicative ( (<$>) )
import Data.List ( intersect )

data Processor = Processor {
   procLibrary :: OpenCLLibrary,
   procContext :: CLContext,
   procDevice  :: CLDeviceID,
   procQueue   :: CLCommandQueue,
   procID      :: String,
   procName    :: String,
   procVendor  :: String,
   procCapabilities :: Set ProcessorCapability
}

instance Eq Processor where
  (==) p1 p2 = procID p1 == procID p2

instance Ord Processor where
  compare p1 p2 = compare (procID p1) (procID p2)

instance Show Processor where
  show p = "{" ++ procID p ++ "}"

-- | Initialize an OpenCL processor
initProc :: OpenCLLibrary -> CLContext -> CLDeviceID -> (Int,Int) -> IO Processor
initProc lib ctx dev (pfIdx,devIdx) = do
   devProps <- clGetDeviceQueueProperties lib dev
   let props = intersect [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE] devProps

   cq <- clCreateCommandQueue lib ctx dev props
   name <- clGetDeviceName lib dev
   vendor <- clGetDeviceVendor lib dev
   let pid = printf "OpenCL %d %d" pfIdx devIdx
   caps <- retrieveCapabilities lib dev

   return $ Processor {
      procLibrary = lib,
      procContext = ctx,
      procDevice = dev,
      procQueue = cq,
      procID = pid,
      procName = name,
      procVendor = vendor,
      procCapabilities = caps
   }

-- | Retrieve capabilities of an OpenCL processor
retrieveCapabilities :: OpenCLLibrary -> CLDeviceID -> IO (Set ProcessorCapability)
retrieveCapabilities lib dev = do
   extensions <- clGetDeviceExtensions lib dev
   return . fromList $
      if "cl_khr_fp64" `elem` extensions 
         then [DoubleFloatingPoint]
         else []

-- | Retrieve attached memory
procMemories :: Processor -> IO [Memory]
procMemories p = return <$> initMemory (procLibrary p) (procContext p) (procDevice p)


-- | Compile a program
programCompile :: CLProgram -> [Processor] -> IO [CompilationResult]
programCompile p procs = undefined

