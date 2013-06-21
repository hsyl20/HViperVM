{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo, hostMemories,
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

import Text.Printf

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
   (hostMems,hostLinks) <- initHost config

   -- Initialize OpenCL driver
   (clMems,clLinks,clProcs) <- initOpenCL config hostMems

   let mems = fmap HostMemory hostMems ++ fmap CLMemory clMems 
       lnks = fmap HostLink hostLinks  ++ fmap CLLink clLinks
       procs = fmap CLProcessor clProcs

   return $ Platform mems lnks procs config

-- | Retrieve platform information string
platformInfo :: Platform -> String
platformInfo pf = printf "Processors:\n%sMemories\n%sLinks\n%s" procs mems lks
   where
     procs = f $ fmap procInfo (processors pf)
     mems = f $ fmap memoryInfo (memories pf)
     lks = f $ fmap linkInfo (links pf)
     f :: [String] -> String
     f = concatMap (\x -> printf "  - %s\n" x)

-- |  Retrieve host memories
hostMemories :: Platform -> [Memory]
hostMemories pf = filter f (memories pf)
   where
      f (HostMemory _) = True
      f _ = False

-- | Put custom message in log
customLog :: Platform -> String -> IO ()
customLog pf s = logger (configuration pf) (CustomLog s)

-- | Put error message in log
errorLog :: Platform -> String -> IO ()
errorLog pf s = logger (configuration pf) (ErrorLog s)

-- | Put debug message in log
debugLog :: Platform -> String -> IO ()
debugLog pf s = logger (configuration pf) (DebugLog s)
