-- This program displays information about the platform

import ViperVM.Platform

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so"
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Querying platform infos..."
  putStr =<< platformInfo platform