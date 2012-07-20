module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import Data.Traversable
import Data.Functor

data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor]
}

data Memory = CLMemory OpenCLLibrary CLContext CLDeviceID | HostMemory
data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory deriving (Eq,Ord)
data Processor = CLProcessor OpenCLLibrary CLContext CLDeviceID | HostProcessor

instance Eq Memory where
  (==) HostMemory HostMemory = True
  (==) (CLMemory _ _ m1) (CLMemory _ _ m2) = (==) m1 m2
  (==) _ _ = False

instance Ord Memory where
  compare HostMemory HostMemory = EQ
  compare (CLMemory _ _ m1) (CLMemory _ _ m2) = compare m1 m2
  compare HostMemory _ = GT
  compare _ HostMemory = LT

-- | Initialize platform
initPlatform :: String -> IO Platform
initPlatform cllib = do
  lib <- loadOpenCL cllib
  platforms <- clGetPlatformIDs lib
  devices <- traverse (clGetDeviceIDs lib CL_DEVICE_TYPE_ALL) platforms
  let platformDevices = zip platforms devices
  contexts <- traverse (\(pf,devs) -> clCreateContext lib [CL_CONTEXT_PLATFORM pf] devs putStrLn) platformDevices
  let deviceContexts = zip devices contexts
  let procs = concat $ (\(devs,ctx) -> fmap (CLProcessor lib ctx) devs) <$> deviceContexts
  queues <- traverse (\(CLProcessor _ ctx dev) -> clCreateCommandQueue lib ctx dev [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, CL_QUEUE_PROFILING_ENABLE]) procs
  let mems = fmap (\(CLProcessor _ ctx dev) -> CLMemory lib ctx dev) procs
  let ls = zipWith (\mem cq -> CLLink lib cq HostMemory mem) mems queues
  return $ Platform mems ls procs


procInfo :: Processor -> IO String
procInfo (CLProcessor lib _ dev) = do
  name <- clGetDeviceName lib dev
  vendor <- clGetDeviceVendor lib dev
  return $ "[OpenCL] " ++ name ++ " (" ++ vendor ++ ")"

platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  return ("Processors:\n" ++ procs)
