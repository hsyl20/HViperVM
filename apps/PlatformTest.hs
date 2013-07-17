-- This program displays information about the platform

import ViperVM.Platform
import ViperVM.Common.Logger

import Data.Traversable (forM)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Control.Applicative ( (<$>) )
import Text.Printf (printf)

main :: IO ()
main = do
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so",
      logger = stdOutLogger . filterLevel LogDebug
   }

   putStrLn "Initializing platform..."
   platform <- initPlatform config

   putStrLn "Querying platform infos..."
   putStrLn (platformInfo platform)

   putStrLn "Allocate a buffer in each memory..."
   let mems = memories platform

   buffers <- catMaybes <$> forM mems (bufferAllocate 1024)
   putStrLn (printf "%d buffers have been allocated" (length buffers))

   putStrLn "Release buffers..."
   forM_ buffers bufferRelease
