module ViperVM.Backends.OpenCL.Driver (
   initOpenCL
   ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse)
import Control.Applicative ( (<$>) )

import ViperVM.Platform.Configuration
import ViperVM.Platform.LinkCapabilities

import qualified ViperVM.Backends.Host.Memory as Host

import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query
import ViperVM.Backends.OpenCL.Context
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Processor
import ViperVM.Backends.OpenCL.Link
import ViperVM.Backends.OpenCL.Memory

-- | Initialize the OpenCL platform
initOpenCL :: Configuration -> [Host.Memory] -> IO ([Memory],[Link],[Processor])
initOpenCL config hostMems = do
   -- Load OpenCL library
   lib <- loadOpenCL (libraryOpenCL config)
   platforms <- clGetPlatformIDs lib

   mlps <- traverse (initOpenCLPlatform lib hostMems) (platforms `zip` [0..])

   return (foldr (\(a,b,c) (a',b',c') -> (a++a',b++b',c++c')) ([],[],[]) mlps)

-- | Initialize an OpenCL platform
initOpenCLPlatform :: OpenCLLibrary -> [Host.Memory] -> (CLPlatformID,Int) -> IO ([Memory],[Link],[Processor])
initOpenCLPlatform lib hostMems (platform,pfIdx) = do
   devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
   context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
   memories  <- initMemories lib context devices
   links <- concat <$> traverse (initLinks lib context hostMems) (devices `zip` memories)
   procs <- traverse (createProc lib pfIdx context) (zip3 devices memories [0..])
   return (memories, links, procs)
   

-- | Initialize OpenCL platform memories
initMemories :: OpenCLLibrary -> CLContext -> [CLDeviceID] -> IO [Memory]
initMemories lib ctx = traverse $ \dev -> do
   initMemory lib ctx dev

-- | Create links for a given device
initLinks :: OpenCLLibrary -> CLContext -> [Host.Memory] -> (CLDeviceID,Memory) -> IO [Link]
initLinks lib ctx hostMems (dev,mem) = do
   let props = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE] -- FIXME: Ensure that out-of-order mode is supported

   queue <- clCreateCommandQueue lib ctx dev props
   caps <- computeLinkCapabilities lib dev
   
   let lks = concatMap (\hm -> [HostCLLink lib queue caps hm mem, CLHostLink lib queue caps mem hm]) hostMems

   return (CLLoopBackLink lib queue caps mem : lks)

-- | Compute link capabilities of a device
computeLinkCapabilities :: OpenCLLibrary -> CLDeviceID -> IO (Set LinkCapability)
computeLinkCapabilities lib dev = do
   transfer2D <- clGetDevice2DTransferSupport lib dev
   return . Set.fromList $ (if transfer2D then [Transfer2D] else [])




-- | Create processor
createProc :: OpenCLLibrary -> Int -> CLContext -> (CLDeviceID, Memory, Int) -> IO Processor
createProc lib pfIdx ctx (dev,_,devIdx) = do
   initProc lib ctx dev (pfIdx,devIdx)

