module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo,
   memories, links, processors
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.Platform.Processor

import Data.Traversable
import Control.Applicative

-- | A computing platform
data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor]
}


-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = do

   -- Load OpenCL platform
   lib <- loadOpenCL (libraryOpenCL config)

   platforms <- clGetPlatformIDs lib

   (clMems, clLinks, clProcs) <- liftA (unzip3 . concat) $ forM platforms $ \platform -> do
      devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
      context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
      forM devices $ \device -> do
         let queueProps = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, CL_QUEUE_PROFILING_ENABLE]
         queue <- clCreateCommandQueue lib context device queueProps
         let mem = CLMemory lib context device
         let link = [CLLink lib queue HostMemory mem, CLLink lib queue mem HostMemory]
         let proc = CLProcessor lib context queue device
         return (mem,link,proc)


   let mems = HostMemory : clMems 
       lnks = concat clLinks
       procs = clProcs

   return $ Platform mems lnks procs

-- | Retrieve platform information string
platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  mems <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse memInfo (memories pf)
  lks <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse linkInfo (links pf)
  return ("Processors:\n" ++ procs ++ "Memories:\n" ++ mems ++ "Links:\n" ++ lks)
