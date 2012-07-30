import Control.Monad ( liftM2 )
import Data.Maybe (isJust)
import Data.Traversable
import Foreign.Marshal.Alloc
import System.IO (stdout)

import ViperVM
import ViperVM.Data
import ViperVM.Kernel
import ViperVM.Library.MatAdd
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
  runtime <- startRuntime platform logger defaultScheduler

  v1 <- sync $ mapVector runtime PrimFloat n v1ptr
  v2 <- sync $ mapVector runtime PrimFloat n v2ptr
  v3 <- sync $ createVector runtime PrimFloat n

  let task = Task floatMatrixAdd [v1,v2,v3] []

  submitTask runtime task

  sync $ stopRuntime runtime

  putStrLn "Done."
