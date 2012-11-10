import Foreign.Marshal.Alloc
import System.IO (stdout)

import ViperVM.Data
import ViperVM.Library.FloatMatrixAdd
import ViperVM.Logging.TextLogger
import ViperVM.Platform
import ViperVM.Runtime
import ViperVM.Scheduling.Default

main :: IO ()
main = do
  let config = Configuration {
    libraryOpenCL = "/usr/lib/libOpenCL.so"
  }

  putStrLn "Initializing platform..."
  platform <- initPlatform config

  putStrLn "Querying platform infos..."
  putStr =<< platformInfo platform

  let n = 1024
  v1ptr <- mallocBytes (fromIntegral $ n*4)
  v2ptr <- mallocBytes (fromIntegral $ n*4)

  logger <- newTextLogger stdout
  scheduler <- defaultScheduler logger
  runtime <- startRuntime platform logger scheduler

  a <- sync $ mapVector runtime PrimFloat n v1ptr
  b <- sync $ mapVector runtime PrimFloat n v2ptr

  [c] <- sync $ submitTask runtime floatMatrixAdd [a,b]

  sync $ waitForData runtime c

  sync $ stopRuntime runtime

  putStrLn "Done."
