-- This program displays information about the platform

import ViperVM.Platform
import ViperVM.Common.Logger

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
