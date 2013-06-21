module ViperVM.Backends.OpenCL.Driver (
   initOpenCL
   ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse, forM)
import Control.Applicative ( (<$>) )

import ViperVM.Platform.Configuration
import ViperVM.Platform.LinkCapabilities
import qualified ViperVM.Platform.Memory as PF
import qualified ViperVM.Backends.Host.Memory as Host
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
createLinks :: OpenCLLibrary -> CLContext -> [Host.Memory] -> (CLDeviceID,Memory) -> IO [Link]
createLinks lib ctx hostMems (dev,mem) = do
   let props = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE] -- FIXME: Ensure that out-of-order mode is supported

   queue <- clCreateCommandQueue lib ctx dev props
   caps <- computeLinkCapabilities lib dev
   

   let
      m = PF.CLMemory mem
      lks = concatMap (\hm -> [
               Link lib queue (PF.HostMemory hm) m caps,
               Link lib queue m (PF.HostMemory hm) caps]) hostMems

   return (Link lib queue m m caps : lks)

-- | Create processor
createProc :: OpenCLLibrary -> Int -> CLContext -> (CLDeviceID, Memory, Int) -> IO Processor
createProc lib pfIdx ctx (dev,mem,devIdx) = do
   initProc lib ctx dev mem (pfIdx,devIdx)

-- | Initialize an OpenCL platform
initOpenCLPlatform :: OpenCLLibrary -> [Host.Memory] -> (CLPlatformID,Int) -> IO ([Memory],[Link],[Processor])
initOpenCLPlatform lib hostMems (platform,pfIdx) = do
   devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
   context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
   memories  <- forM devices (initMemory lib context)

   procs <- traverse (createProc lib pfIdx context) (zip3 devices memories [0..])

   links <- concat <$> traverse (createLinks lib context hostMems) (devices `zip` memories)

   return (memories, links, procs)
   

-- | Initialize the OpenCL platform
initOpenCL :: Configuration -> [Host.Memory] -> IO ([Memory],[Link],[Processor])
initOpenCL config hostMems = do
   -- Load OpenCL library
   lib <- loadOpenCL (libraryOpenCL config)
   platforms <- clGetPlatformIDs lib

   mlps <- traverse (initOpenCLPlatform lib hostMems) (platforms `zip` [0..])

   return (foldr (\(a,b,c) (a',b',c') -> (a++a',b++b',c++c')) ([],[],[]) mlps)
