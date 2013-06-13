{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo,
   memories, links, processors, configuration,
   customLog, errorLog, debugLog
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.Platform.Processor
import ViperVM.Platform.Logger

import Data.Traversable
import Control.Applicative
import Text.Printf

-- | A computing platform
data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor],
  configuration :: Configuration
}


-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String,
  logger :: LogMsg -> IO ()
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = do

   -- Load OpenCL platform
   lib <- loadOpenCL (libraryOpenCL config)

   platforms <- clGetPlatformIDs lib
   let indexes = [0..] :: [Integer]

   (clMems, clLinks, clProcs) <- liftA (unzip3 . concat) $ forM (platforms `zip` indexes) $ \(platform,pfIdx) -> do
      devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
      context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
      forM (devices `zip` indexes) $ \(device,devIdx) -> do
         let queueProps = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, CL_QUEUE_PROFILING_ENABLE]
         queue <- clCreateCommandQueue lib context device queueProps
         let mem = CLMemory lib context device
         let link = [CLLink lib queue HostMemory mem, CLLink lib queue mem HostMemory]
         let proc = CLProcessor lib context queue device (printf "OpenCL %d %d" pfIdx devIdx)
         return (mem,link,proc)


   let mems = HostMemory : clMems 
       lnks = concat clLinks
       procs = clProcs

   return $ Platform mems lnks procs config

-- | Retrieve platform information string
platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  mems <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse memInfo (memories pf)
  lks <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse linkInfo (links pf)
  return ("Processors:\n" ++ procs ++ "Memories:\n" ++ mems ++ "Links:\n" ++ lks)


-- | Put custom message in log
customLog :: Platform -> String -> IO ()
customLog pf s = logger (configuration pf) (CustomLog s)

-- | Put error message in log
errorLog :: Platform -> String -> IO ()
errorLog pf s = logger (configuration pf) (ErrorLog s)

-- | Put debug message in log
debugLog :: Platform -> String -> IO ()
debugLog pf s = logger (configuration pf) (DebugLog s)
