import ViperVM
import ViperVM.Platform
import ViperVM.Runtime
import ViperVM.Data
import Data.Traversable

import Foreign.Marshal.Alloc

main = do
  let cllib = "/usr/lib/libOpenCL.so"

  platform <- initPlatform cllib
  putStrLn =<< platformInfo platform

  putStrLn "Starting the runtime..."
  runtime <- startRuntime platform

  let n = 1024
  v1ptr <- mallocBytes (fromIntegral $ n*4)
  v2ptr <- mallocBytes (fromIntegral $ n*4)

  v1 <- mapVector runtime (VectorDesc PrimFloat n) v1ptr
  v2 <- mapVector runtime (VectorDesc PrimFloat n) v2ptr

  putStrLn "Stopping the runtime..."
  stopRuntime runtime

  putStrLn "Done."
