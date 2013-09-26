-- This program displays information about the platform

import ViperVM.Platform.Configuration
import ViperVM.Platform.Memory
import ViperVM.Platform.Platform
import ViperVM.Runtime.Logger
import ViperVM.Backends.Common.Buffer

import Data.Traversable (traverse)
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Control.Applicative ( (<$>) )
import Text.Printf (printf)

main :: IO ()
main = do
   let 
      logger = stdOutLogger . filterLevel LogDebug
      config = Configuration {
         libraryOpenCL = "/usr/lib/libOpenCL.so",
         eventHandler = \e -> logger (CustomLog (show e))
      }

   putStrLn "Initializing platform..."
   platform <- initPlatform config

   putStrLn "Querying platform infos..."
   putStrLn (platformInfo platform)

   putStrLn "Allocate a buffer in each memory..."
   let mems = memories platform
       f (AllocSuccess x) = Just x
       f AllocError = Nothing

   buffers <- catMaybes <$> traverse (\x -> f <$> bufferAllocate 1024 x) mems
   putStrLn (printf "%d buffers have been allocated" (length buffers))

   putStrLn "Release buffers..."
   forM_ buffers bufferRelease

