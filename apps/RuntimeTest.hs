import Foreign.Marshal.Alloc

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Platform
import ViperVM.Runtime.DefaultScheduler

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

  runtime <- createRuntime platform 

  registerFloatMatrixAdd runtime

  a <- mapVectorIO runtime PrimFloat n v1ptr
  b <- mapVectorIO runtime PrimFloat n v2ptr

  [c] <- submitTaskIO runtime floatMatrixAdd [a,b]

  waitDataIO runtime [c]

  putStrLn "Done."
