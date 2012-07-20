import ViperVM
import ViperVM.Platform
import ViperVM.Runtime
import ViperVM.Data
import ViperVM.Kernel

import ViperVM.Library.MatAdd

import Control.Monad ( liftM2 )
import Data.Traversable
import Data.Maybe (isJust)

import Foreign.Marshal.Alloc

main = do
  let cllib = "/usr/lib/libOpenCL.so"

  putStrLn "Querying platform infos..."
  platform <- initPlatform cllib
  putStrLn =<< platformInfo platform

  let n = 1024
  v1ptr <- mallocBytes (fromIntegral $ n*4)
  v2ptr <- mallocBytes (fromIntegral $ n*4)

  putStrLn "Compiling OpenCL Matrix addition kernel for the platform"
  compiled <- compileKernels matrixAddCL (processors platform)
  let cca = fmap (\c -> if isJust c then "  - Compiled successfully for " else "  - Compilation failed for ") compiled
  ccb <- traverse procInfo (processors platform)
  traverse putStrLn $ zipWith (++) cca ccb
  putStrLn ""

  putStrLn "Starting the runtime..."
  runtime <- startRuntime platform

  v1 <- mapVector runtime (VectorDesc PrimFloat n) v1ptr
  v2 <- mapVector runtime (VectorDesc PrimFloat n) v2ptr

  putStrLn "Stopping the runtime..."
  stopRuntime runtime

  putStrLn "Done."
