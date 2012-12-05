module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import Data.Traversable
import Control.Applicative
import System.IO.Unsafe
import Text.Printf

data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor]
}

data Memory = HostMemory
            | CLMemory OpenCLLibrary CLContext CLDeviceID

data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory deriving (Eq,Ord)

instance Show Link where
  show (CLLink _ _ m1 m2) = printf "OpenCL link between %s and %s" (show m1) (show m2)

getLinkMemories :: Link -> (Memory,Memory)
getLinkMemories (CLLink _ _ m1 m2) = (m1,m2)

data Processor = HostProcessor
               | CLProcessor OpenCLLibrary CLContext CLCommandQueue CLDeviceID

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
  (==) (CLProcessor lib1 _ _ id1) (CLProcessor lib2 _ _ id2) = lib1 == lib2 && id1 == id2
  (==) _ _ = False

instance Ord Processor where
  compare HostProcessor HostProcessor = EQ
  compare HostProcessor _ = GT
  compare _ HostProcessor = LT
  compare (CLProcessor _ _ _ id1) (CLProcessor _ _ _ id2) = compare id1 id2

instance Show Processor where
  show p = unsafePerformIO $ procInfo p


data Configuration = Configuration {
  libraryOpenCL :: String
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = do
   let cllib = libraryOpenCL config 
   lib <- loadOpenCL cllib
   platforms <- clGetPlatformIDs lib
   liftA (platformFromTuple . unzip3 . concat) $ forM platforms $ \platform -> do
      devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
      context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
      forM devices $ \device -> do
         let queueProps = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, CL_QUEUE_PROFILING_ENABLE]
         queue <- clCreateCommandQueue lib context device queueProps
         let mem = CLMemory lib context device
         let link = CLLink lib queue HostMemory mem
         let proc = CLProcessor lib context queue device
         return (mem,link,proc)
   where
      platformFromTuple (mems,lnks,procs) = Platform mems lnks procs


procInfo :: Processor -> IO String
procInfo (CLProcessor lib _ _ dev) = do
  name <- clGetDeviceName lib dev
  vendor <- clGetDeviceVendor lib dev
  return $ "[OpenCL] " ++ name ++ " (" ++ vendor ++ ")"

procInfo HostProcessor = return "[Host]"

platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  return ("Processors:\n" ++ procs)


memInfo :: Memory -> IO String
memInfo HostMemory = return "Host Memory"
memInfo (CLMemory lib _ dev) = do
  sz <- clGetDeviceGlobalMemSize lib dev
  return $ printf "OpenCL Memory %s (%s KB)" (show dev) (show sz)

attachedMemories :: Processor -> [Memory]
attachedMemories (CLProcessor lib ctx _ dev) = [CLMemory lib ctx dev]
attachedMemories HostProcessor = [HostMemory]

