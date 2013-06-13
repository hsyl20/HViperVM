import ViperVM.Platform
import ViperVM.Platform.Logger
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager (createBufferManager)

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

  putStrLn "Initializing transfer manager..."
  bm <- createBufferManager platform
  rm <- createRegionLockManager bm
  tm <- createRegionTransferManager rm

  let bufferSize = 1024 * 1024

  putStrLn $ printf "\nTrying to transfer %d KB on each link..." (bufferSize `div` 1024)
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link
      Just srcBuf <- allocateBuffer rm src bufferSize
      Just dstBuf <- allocateBuffer rm dst bufferSize
      let reg = Region1D 0 bufferSize

      putStrLn $ "Transferring on " ++ (show link) ++ "... "
      let tr = RegionTransfer srcBuf reg [RegionTransferStep link dstBuf reg]

      putStr " - Preparing transfer... "
      res1 <- prepareRegionTransferIO tm tr
      if res1 /= PrepareSuccess
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Performing transfer... "
      res2 <- performRegionTransfer tm tr
      if any (/= RegionTransferSuccess) res2
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Preparing transfer... "
      res3 <- prepareRegionTransferIO tm tr
      if res3 /= PrepareSuccess
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      putStr " - Performing transfer... "
      res4 <- performRegionTransfer tm tr
      if any (/= RegionTransferSuccess) res4
         then putStrLn "ERROR"
         else putStrLn "SUCCEEDED"

      void $ releaseBuffer rm srcBuf
      void $ releaseBuffer rm dstBuf


  putStrLn $ printf "\nTrying to perform multi-step (ping pong) transfer of %d KB on each couple of links..." (bufferSize `div` 1024)
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link

      -- Find associated reverse link
      let r = filter (\x -> linkEndpoints x == (dst,src)) (links platform)
      forM r $ \link2 -> do
         Just srcBuf <- allocateBuffer rm src bufferSize
         Just step1Buf <- allocateBuffer rm dst bufferSize
         Just step2Buf <- allocateBuffer rm src bufferSize
         Just dstBuf <- allocateBuffer rm dst bufferSize
         let reg = Region1D 0 bufferSize

         putStrLn $ "Ping-pong through " ++ show link ++ " and " ++ show link2 ++ "... "
         let tr = RegionTransfer srcBuf reg [
                     RegionTransferStep link step1Buf reg,
                     RegionTransferStep link2 step2Buf reg,
                     RegionTransferStep link dstBuf reg
                  ]

         res <- performRegionTransfer tm tr
         if any (/= RegionTransferSuccess) res
            then putStrLn "ERROR"
            else putStrLn "SUCCEEDED"

         forM_ [srcBuf, step1Buf, step2Buf, dstBuf] (releaseBuffer rm)

  putStrLn "Done."
