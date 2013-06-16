import ViperVM.Platform
import ViperVM.Platform.Logger
import ViperVM.Platform.TransferResult
import ViperVM.Platform.LinkCapabilities
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager (createBufferManager)

import Control.Monad
import Text.Printf
import qualified Data.Set as Set

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

  let bSize = 1024 * 1024

  putStrLn $ printf "\nTrying to transfer %d KB on each link..." (bSize `div` 1024)
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link
      Just srcBuf <- allocateBuffer rm src bSize
      Just dstBuf <- allocateBuffer rm dst bSize
      let reg = Region1D 0 bSize

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


  putStrLn $ printf "\nTrying to perform multi-step (ping pong) transfer of %d KB on each couple of links..." (bSize `div` 1024)
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link

      -- Find associated reverse link
      let r = filter (\x -> linkEndpoints x == (dst,src)) (links platform)
      forM r $ \link2 -> do
         Just srcBuf <- allocateBuffer rm src bSize
         Just step1Buf <- allocateBuffer rm dst bSize
         Just step2Buf <- allocateBuffer rm src bSize
         Just dstBuf <- allocateBuffer rm dst bSize
         let reg = Region1D 0 bSize

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

  putStrLn "\nTrying to perform 2D transfer transfer on each link..."
  forM_ (links platform) $ \link -> do
      let (src,dst) = linkEndpoints link

      if Set.notMember Transfer2D (linkCapabilities link)
         then putStrLn ("2D transfers not supported by link " ++ show link)
         else do
            let reg2D = Region2D 0 32 30 2 -- 2 bytes of padding, 32 bytes aligned
                bsize = 32*32

            Just srcBuf <- allocateBuffer rm src bsize
            Just dstBuf <- allocateBuffer rm dst bsize

            putStrLn $ "Performing 2D transfer through " ++ show link
            let tr = RegionTransfer srcBuf reg2D [
                        RegionTransferStep link dstBuf reg2D
                     ]
            res <- performRegionTransfer tm tr
            if any (/= RegionTransferSuccess) res
               then putStrLn "ERROR"
               else putStrLn "SUCCEEDED"

            forM_ [srcBuf, dstBuf] (releaseBuffer rm)


  putStrLn "Done."
