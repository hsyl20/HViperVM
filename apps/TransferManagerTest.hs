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

      putStrLn $ "Transferring on " ++ (show link) ++ "... "
      let tr = Transfer srcBuf reg [TransferStep link dstBuf reg]

      putStr " - Preparing transfer... "
      res1 <- prepareTransferIO tm tr
      if res1 /= PrepareSuccess
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Performing transfer... "
      res2 <- performTransfer tm tr
      if any (/= TransferSuccess) res2
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Preparing transfer... "
      res3 <- prepareTransferIO tm tr
      if res3 /= PrepareSuccess
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Performing transfer... "
      res4 <- performTransfer tm tr
      if any (/= TransferSuccess) res4
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      void $ releaseBuffer rm srcBuf
      void $ releaseBuffer rm dstBuf

  putStrLn "Done."
