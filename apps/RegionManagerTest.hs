import ViperVM.Platform
import ViperVM.Platform.BufferManager (createBufferManager)
import ViperVM.Platform.RegionManager

import Control.Monad
import Control.Concurrent.STM

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so"
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Initializing region manager..."
  bm <- createBufferManager platform
  rm <- createRegionManager bm

  let bufferSize = 1024 * 1024
      r1 = Region1D 100 500
      r2 = Region2D 50 10 100 50

  forM_ (memories platform) $ \mem -> do
      putStrLn =<< memInfo mem
      putStrLn " - Allocating buffer... "
      buf <- allocateBuffer rm mem bufferSize
      case buf of
         Nothing -> putStrLn "  FAILED"
         Just b  -> do
            putStrLn " - Locking regions... "
            res1 <- atomically $ lockRegion rm b r1 ReadOnly
            res2 <- atomically $ lockRegion rm b r1 ReadOnly
            res3 <- atomically $ lockRegion rm b r1 ReadWrite
            res6 <- atomically $ lockRegion rm b r2 ReadWrite
            res4 <- releaseBuffer rm b
            atomically $ unlockRegion rm b r1 ReadOnly
            atomically $ unlockRegion rm b r1 ReadOnly
            res5 <- releaseBuffer rm b
            if (res1 == LockSuccess &&
                res2 == LockSuccess && 
                res3 == RegionAlreadyLocked && 
                res4 == RemainingRegion && 
                res5 == BufferReleaseSuccess &&
                res6 == RegionAlreadyLocked)
               then putStrLn "  SUCCEEDED"
               else putStrLn "  FAILED"
                  

  

  putStrLn "Done."
