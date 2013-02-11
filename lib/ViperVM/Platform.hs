-- | This module gives applications a complete view of the underlying
-- architecture (memory network and processors)
module ViperVM.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo,
   memories, links, processors,
   module ViperVM.Platform.Memory,
   module ViperVM.Platform.Processor,
   module ViperVM.Platform.Link,
   module ViperVM.Platform.Buffer,
   module ViperVM.Platform.Region,
   module ViperVM.Platform.Kernel,
   module ViperVM.Platform.DataInstance,
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory
import ViperVM.Platform.Processor
import ViperVM.Platform.Link
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.Kernel
import ViperVM.Platform.DataInstance

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
         let link = CLLink lib queue HostMemory mem
         let proc = CLProcessor lib context queue device
         return (mem,link,proc)


   let mems = HostMemory : clMems 
       lnks = clLinks
       procs = clProcs

   return $ Platform mems lnks procs

-- | Retrieve platform information string
platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  mems <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse memInfo (memories pf)
  lks <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse linkInfo (links pf)
  return ("Processors:\n" ++ procs ++ "Memories:\n" ++ mems ++ "Links:\n" ++ lks)
