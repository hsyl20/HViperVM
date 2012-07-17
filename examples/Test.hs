import ViperVM
import ViperVM.Platform
import Data.Traversable

main = do
  let cllib = "/usr/lib/libOpenCL.so"
  putStrLn $ "Loading OpenCL library: " ++ cllib
  cl <- loadOpenCL cllib

  -- Show platforms
  putStrLn "Platforms:"
  platforms <- clGetPlatformIDs cl
  platformNames <- traverse (clGetPlatformInfo cl CL_PLATFORM_NAME) platforms
  platformVendors <- traverse (clGetPlatformInfo cl CL_PLATFORM_VENDOR) platforms
  traverse (putStrLn) (zipWith (\x y -> x ++ " - " ++ y) platformNames platformVendors)
  putStrLn "-------------"

  -- Show devices
  putStrLn "Devices:"
  devices <- traverse (clGetDeviceIDs cl CL_DEVICE_TYPE_ALL) platforms
  deviceNames <- traverse (traverse (clGetDeviceName cl)) devices
  traverse (traverse (putStrLn)) deviceNames
  putStrLn "-------------"

  platform <- initPlatform cllib

  putStrLn "Done."
