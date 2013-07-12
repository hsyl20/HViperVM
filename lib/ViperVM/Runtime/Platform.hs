{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo, hostMemories,
   memories, links, processors, configuration,
   customLog, errorLog, debugLog
) where

import qualified ViperVM.Backends.Host.Driver as Host
import qualified ViperVM.Backends.OpenCL.Driver as CL

import ViperVM.Platform.Memory
import ViperVM.Platform.MemoryPeer
import ViperVM.Platform.Link
import ViperVM.Platform.LinkPeer
import ViperVM.Platform.Processor
import ViperVM.Platform.Logger
import ViperVM.Platform.Configuration

import Data.List
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
   (hostMems,hostLinks) <- Host.initHost config

   let hostMemsPeer = fmap HostMemory hostMems
       hostLinksPeer = fmap HostLink hostLinks

   -- Initialize OpenCL driver
   (clMems,clLinks,clProcs) <- initOpenCL config hostMems

   let clMemsPeer = fmap CLMemory clMems
       clLinksPeer = fmap CLLink clLinks
       clProcsPeer = fmap CLProcessor clProcs

   -- Wrap entities
   let memsPeer = hostMemsPeer ++ clMemsPeer
       linksPeer = hostLinksPeer ++ clLinksPeer
       procsPeer = clProcsPeer

   mems <- forM (memsPeers `zip` [0..]) (uncurry wrapMemory)
   links <- forM linksPeers (wrapLink mems)
   let 
       procs = fmap (flip CLProcessor clMems) clProcs

   return $ Platform mems links procs config

-- | Retrieve platform information string
platformInfo :: Platform -> String
platformInfo pf = printf "Processors:\n%s\nMemories\n%s\nLinks\n%s" procs mems lks
   where
     procs = f $ fmap procInfo (processors pf)
     mems = f $ fmap memoryInfo (memories pf)
     lks = f $ fmap linkInfo (links pf)

     f :: [String] -> String
     f = intercalate "\n" . fmap (\x -> printf "  - %s" x)

-- |  Retrieve host memories
hostMemories :: Platform -> [Memory]
hostMemories pf = filter f (memories pf)
   where
      f (HostMemory {}) = True
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
