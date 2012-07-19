import ViperVM
import ViperVM.Platform
import Data.Traversable

main = do
  let cllib = "/usr/lib/libOpenCL.so"

  platform <- initPlatform cllib
  putStrLn =<< platformInfo platform

  putStrLn "Done."
