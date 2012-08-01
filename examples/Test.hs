import Control.Monad ( liftM2 )
import Data.Maybe (isJust)
import Data.Traversable
import Foreign.Marshal.Alloc
import System.IO (stdout)

import ViperVM
import ViperVM.Data
import ViperVM.Kernel
import ViperVM.Library.FloatMatrixAdd
import ViperVM.Logging.TextLogger
import ViperVM.Platform
import ViperVM.Runtime
import ViperVM.Scheduling.Default
import ViperVM.Task

main = do
  let cllib = "/usr/lib/libOpenCL.so"

  putStrLn "Querying platform infos..."
  platform <- initPlatform cllib
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

  sync $ stopRuntime runtime

  putStrLn "Done."
