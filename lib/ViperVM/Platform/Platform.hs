{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Platform (
   Platform, Configuration(..),
   initPlatform, platformInfo,
   memories, links, processors, configuration,
   customLog, errorLog, debugLog
) where

import qualified ViperVM.Platform.Peer.PlatformPeer as Peer

import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.Platform.Proc
import ViperVM.Platform.Configuration
import ViperVM.Common.Logger

import Data.List
import Text.Printf
import Control.Applicative ( (<$>) )

-- | A computing platform
data Platform = Platform {
   platformPeer :: Peer.PlatformPeer
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = Platform <$> Peer.initPlatform config

-- | Platform processors
processors :: Platform -> [Proc]
processors = Peer.processors . platformPeer

-- | Platform memories
memories :: Platform -> [Memory]
memories = Peer.memories . platformPeer

-- | Platform links
links :: Platform -> [Link]
links = Peer.links . platformPeer

-- | Platform configuration
configuration :: Platform -> Configuration
configuration = Peer.configuration . platformPeer

-- | Retrieve platform information string
platformInfo :: Platform -> String
platformInfo pf = printf "Processors:\n%s\nMemories\n%s\nLinks\n%s" procs mems lks
   where
     procs = f $ fmap procInfo (processors pf)
     mems = f $ fmap memoryInfo (memories pf)
     lks = f $ fmap linkInfo (links pf)

     f :: [String] -> String
     f = intercalate "\n" . fmap (\x -> printf "  - %s" x)

-- | Put custom message in log
customLog :: Platform -> String -> IO ()
customLog pf s = logger (configuration pf) (CustomLog s)

-- | Put error message in log
errorLog :: Platform -> String -> IO ()
errorLog pf s = logger (configuration pf) (ErrorLog s)

-- | Put debug message in log
debugLog :: Platform -> String -> IO ()
debugLog pf s = logger (configuration pf) (DebugLog s)
