import ViperVM

import Data.Traversable

main = do
  let cllib = "/usr/lib/libOpenCL.so"
  putStrLn $ "Loading OpenCL library: " ++ cllib
  cl <- loadOpenCL cllib
  platforms <- clGetPlatformIDs cl
  platformNames <- traverse (\x -> clGetPlatformInfo cl x CL_PLATFORM_NAME) platforms
  platformVendors <- traverse (\x -> clGetPlatformInfo cl x CL_PLATFORM_VENDOR) platforms
  traverse (putStrLn) (zipWith (\x y -> x ++ " - " ++ y) platformNames platformVendors)
