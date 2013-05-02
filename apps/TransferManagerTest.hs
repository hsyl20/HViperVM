import ViperVM.Platform
import ViperVM.Platform.TransferManager
import ViperVM.Platform.RegionManager
import ViperVM.Platform.BufferManager (createBufferManager)

import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so"
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Initializing transfer manager..."
  bm <- createBufferManager platform
  rm <- createRegionManager bm
  tm <- createTransferManager rm

  let bufferSize = 1024 * 1024

  putStrLn $ printf "\nTrying to transfer %d KB on each link..." (bufferSize `div` 1024)
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link
      Just srcBuf <- allocateBuffer rm src bufferSize
      Just dstBuf <- allocateBuffer rm dst bufferSize
      let reg = Region1D 0 bufferSize

      let t = Transfer link srcBuf reg dstBuf reg
      putStr $ "Transferring on " ++ (show link) ++ "... "

      ev <- performDirectTransfer tm t
      res <- waitForTransfer tm ev
      case res of
         TransferSuccess -> putStrLn "SUCCEEDED"
         TransferError -> putStrLn "ERROR"

      void $ releaseBuffer rm srcBuf
      void $ releaseBuffer rm dstBuf

  putStrLn "Done."
