-- This program displays information about the platform

import ViperVM.Platform.Configuration
import ViperVM.Platform.Platform
import ViperVM.Runtime.Logger

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
