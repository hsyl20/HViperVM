import ViperVM.Platform
import ViperVM.Platform.Logger
import ViperVM.Platform.BufferManager (createBufferManager)
import ViperVM.Platform.RegionLockManager

import Control.Monad
import Control.Concurrent.STM

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so",
    logger = stdOutLogger LogDebug
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Initializing region manager..."
  bm <- createBufferManager platform
  rm <- createRegionLockManager bm

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
            res1 <- atomically $ lockRegion rm ReadOnly b r1
            res2 <- atomically $ lockRegion rm ReadOnly b r1
            res3 <- atomically $ lockRegion rm ReadWrite b r1
            res6 <- atomically $ lockRegion rm ReadWrite b r2
            res4 <- releaseBuffer rm b
            atomically $ unlockRegion rm ReadOnly b r1
            atomically $ unlockRegion rm ReadOnly b r1
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
