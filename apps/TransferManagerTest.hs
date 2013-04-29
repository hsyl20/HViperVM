import ViperVM.Platform

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
  tm <- createTransferManager platform
  let mm = bufferManager tm

  let bufferSize = 1024 * 1024

  putStrLn $ printf "\nTrying to transfer %d KB on each link..." (bufferSize `div` 1024)
  forM (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link
      Just srcBuf <- allocateBuffer mm src bufferSize
      Just dstBuf <- allocateBuffer mm dst bufferSize
      let reg = Region1D 0 bufferSize

      let t = Transfer link srcBuf reg dstBuf reg
      putStr $ "Transferring on " ++ (show link) ++ "... "

      ev <- submitDirectTransfer tm t
      res <- waitForTransfer tm ev
      case res of
         TransferSuccess -> putStrLn "SUCCEEDED"
         TransferError -> putStrLn "ERROR"

      releaseBuffer mm srcBuf
      releaseBuffer mm dstBuf

  putStrLn "Done."
