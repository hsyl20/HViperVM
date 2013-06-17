module ViperVM.Backends.OpenCL.Driver (
   initOpenCL
   ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse)
import Control.Applicative ( (<$>) )
import Text.Printf

import ViperVM.Platform.Configuration
import ViperVM.Platform.LinkCapabilities
import qualified ViperVM.Platform.Memory as PF
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query
import ViperVM.Backends.OpenCL.Context
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Processor
import ViperVM.Backends.OpenCL.Link
import ViperVM.Backends.OpenCL.Memory

-- | Compute link capabilities of a device
computeLinkCapabilities :: OpenCLLibrary -> CLDeviceID -> IO (Set LinkCapability)
computeLinkCapabilities lib dev = do
   transfer2D <- clGetDevice2DTransferSupport lib dev
   return . Set.fromList $ (if transfer2D then [Transfer2D] else [])


-- | Create links for a given device
createLinks :: OpenCLLibrary -> CLContext -> (CLDeviceID,Memory) -> IO [Link]
createLinks lib ctx (dev,mem) = do
   let props = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE] -- FIXME: Ensure that out-of-order mode is supported

   queue <- clCreateCommandQueue lib ctx dev props
   caps <- computeLinkCapabilities lib dev
   
   return [Link lib queue PF.HostMemory (PF.CLMemory mem) caps, 
           Link lib queue (PF.CLMemory mem) PF.HostMemory caps,
           Link lib queue (PF.CLMemory mem) (PF.CLMemory mem) caps]

-- | Create processor
createProc :: OpenCLLibrary -> Int -> CLContext -> (CLDeviceID, Int) -> IO Processor
createProc lib pfIdx ctx (dev,devIdx) = do
   let props = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE] -- FIXME: Ensure that out-of-order mode is supported
   queue <- clCreateCommandQueue lib ctx dev props
   return (Processor lib ctx queue dev (printf "OpenCL %d %d" pfIdx devIdx))

-- | Initialize an OpenCL platform
initOpenCLPlatform :: OpenCLLibrary -> (CLPlatformID,Int) -> IO ([Memory],[Link],[Processor])
initOpenCLPlatform lib (platform,pfIdx) = do
   devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
   context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
   procs <- traverse (createProc lib pfIdx context) (devices `zip` [0..])

   let mems  = fmap (Memory lib context) devices

   links <- concat <$> traverse (createLinks lib context) (devices `zip` mems)

   return (mems, links, procs)
   

-- | Initialize the OpenCL platform
initOpenCL :: Configuration -> IO ([Memory],[Link],[Processor])
initOpenCL config = do
   -- Load OpenCL library
   lib <- loadOpenCL (libraryOpenCL config)
   platforms <- clGetPlatformIDs lib

   mlps <- traverse (initOpenCLPlatform lib) (platforms `zip` [0..])

   return (foldr (\(a,b,c) (a',b',c') -> (a++a',b++b',c++c')) ([],[],[]) mlps)
