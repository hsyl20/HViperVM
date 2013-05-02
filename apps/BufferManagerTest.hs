import ViperVM.Platform
import ViperVM.Platform.BufferManager
import ViperVM.Event

import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so"
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Querying platform infos..."
  putStr =<< platformInfo platform

  putStrLn "Initializing buffer manager..."
  mm <- createBufferManager platform

  let bufferSize = 1024 * 1024

  putStrLn $ printf "\nTrying to allocate a buffer (%d KB) in each memory node..." (bufferSize `div` 1024)
  buffers <- forM (memories platform) $ \mem -> do
      putStrLn =<< memInfo mem
      buf <- allocateBuffer mm mem bufferSize
      case buf of
         Nothing -> putStrLn "     --> Allocation FAILED"
         Just _  -> putStrLn "     --> Allocation SUCCEEDED"
      return (mem,buf)

  putStrLn "\nTrying to release allocated buffers..."
  forM_ buffers $ \(mem,buf) -> do
      putStrLn =<< memInfo mem
      case buf of 
         Nothing -> putStrLn "     --> Nothing to do"
         Just b -> do
            releaseBuffer mm b
            putStrLn "     --> Release SUCCEEDED"

  putStrLn "Done."
