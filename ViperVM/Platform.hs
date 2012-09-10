module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import Data.Traversable
import Data.Functor
import System.IO.Unsafe

data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor]
}

data Memory = HostMemory
            | CLMemory OpenCLLibrary CLContext CLDeviceID

data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory deriving (Eq,Ord)

getLinkMemories :: Link -> (Memory,Memory)
getLinkMemories (CLLink _ _ m1 m2) = (m1,m2)

data Processor = HostProcessor
               | CLProcessor OpenCLLibrary CLContext CLDeviceID

instance Eq Memory where
  (==) HostMemory HostMemory = True
  (==) (CLMemory _ _ m1) (CLMemory _ _ m2) = (==) m1 m2
  (==) _ _ = False

instance Ord Memory where
  compare HostMemory HostMemory = EQ
  compare (CLMemory _ _ m1) (CLMemory _ _ m2) = compare m1 m2
  compare HostMemory _ = GT
  compare _ HostMemory = LT

instance Show Memory where
  show m = unsafePerformIO $ memInfo m

instance Eq Processor where
  (==) HostProcessor HostProcessor = True
  (==) (CLProcessor lib1 _ id1) (CLProcessor lib2 _ id2) = lib1 == lib2 && id1 == id2
  (==) _ _ = False

instance Ord Processor where
  compare HostProcessor HostProcessor = EQ
  compare HostProcessor _ = GT
  compare _ HostProcessor = LT
  compare (CLProcessor _ _ id1) (CLProcessor _ _ id2) = compare id1 id2

instance Show Processor where
  show p = unsafePerformIO $ procInfo p

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

procInfo HostProcessor = return "[Host]"

platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  return ("Processors:\n" ++ procs)


memInfo :: Memory -> IO String
memInfo HostMemory = return "[Host Memory]"
memInfo (CLMemory lib _ dev) = do
  sz <- clGetMemSize lib dev
  return $ "[OpenCL] " ++ (show sz) ++ "MB"

attachedMemories :: Processor -> [Memory]
attachedMemories (CLProcessor lib ctx dev) = [CLMemory lib ctx dev]
attachedMemories HostProcessor = [HostMemory]

