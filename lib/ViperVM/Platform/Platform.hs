{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo,
   memories, links, processors, configuration,
   customLog, errorLog, debugLog
) where

import ViperVM.Backends.Host.Driver
import ViperVM.Backends.OpenCL.Driver
import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.Platform.Processor
import ViperVM.Platform.Logger
import ViperVM.Platform.Configuration

import Data.Traversable
import Control.Applicative

-- | A computing platform
data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor],
  configuration :: Configuration
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = do

   -- Initialize Host driver
   hostLinks <- initHost config

   -- Initialize OpenCL driver
   (clMems,clLinks,clProcs) <- initOpenCL config

   let mems = HostMemory : fmap CLMemory clMems 
       lnks = fmap HostLink hostLinks ++ fmap CLLink clLinks
       procs = fmap CLProcessor clProcs

   return $ Platform mems lnks procs config

-- | Retrieve platform information string
platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  mems <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse memoryInfo (memories pf)
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
