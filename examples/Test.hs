import ViperVM
import ViperVM.Platform
import ViperVM.Runtime
import ViperVM.Data
import ViperVM.Kernel
import ViperVM.Scheduling.Default

import System.IO (stdout)
import ViperVM.Logging.TextLogger

import ViperVM.Library.MatAdd

import Control.Monad ( liftM2 )
import Data.Traversable
import Data.Maybe (isJust)

import Foreign.Marshal.Alloc

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

  compiled <- sync $ registerKernel runtime matrixAddCL

  sync $ stopRuntime runtime

  putStrLn "Done."
