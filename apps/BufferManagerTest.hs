import ViperVM.Platform
import ViperVM.Platform.Logger
import ViperVM.Platform.BufferManager

import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so",
    logger = stdOutLogger . filterLevel LogDebug
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Querying platform infos..."
  putStr =<< platformInfo platform

  putStrLn "Initializing buffer manager..."
  mm <- createBufferManager platform

  let bSize = 1024 * 1024

  putStrLn $ printf "\nTrying to allocate a buffer (%d KB) in each memory node..." (bSize `div` 1024)
  buffers <- forM (memories platform) $ \mem -> do
      putStrLn =<< memoryInfo mem
      buf <- allocateBuffer mm mem bSize
      case buf of
         Nothing -> putStrLn "     --> Allocation FAILED"
         Just _  -> putStrLn "     --> Allocation SUCCEEDED"
      return (mem,buf)

  putStrLn "\nTrying to release allocated buffers..."
  forM_ buffers $ \(mem,buf) -> do
      putStrLn =<< memoryInfo mem
      case buf of 
         Nothing -> putStrLn "     --> Nothing to do"
         Just b -> do
            releaseBuffer mm b
            putStrLn "     --> Release SUCCEEDED"

  putStrLn "Done."
