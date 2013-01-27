-- | This module gives applications a complete view of the underlying
-- architecture (memory network and processors)
module ViperVM.Platform (
   Platform, Memory(..), Processor(..), Link(..), Configuration(..),
   initPlatform, platformInfo, procInfo, memInfo, linkInfo,
   memories, links, processors,
   linkEndpoints, processorMemories
) where

import ViperVM.Backends.OpenCL
import Data.Traversable
import Control.Applicative
import System.IO.Unsafe
import Text.Printf


-- | A computing platform
data Platform = Platform {
  memories :: [Memory],
  links :: [Link],
  processors :: [Processor]
}


-- | A memory node
data Memory = HostMemory
            | CLMemory OpenCLLibrary CLContext CLDeviceID

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



-- | A link between two memories
data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory
            deriving (Eq,Ord)

instance Show Link where
  show (CLLink _ _ m1 m2) = printf "OpenCL link between %s and %s" (show m1) (show m2)



-- | A processing unit
data Processor = HostProcessor
               | CLProcessor OpenCLLibrary CLContext CLCommandQueue CLDeviceID

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


-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String
}

-- | Initialize platform
initPlatform :: Configuration -> IO Platform
initPlatform config = do

   -- Load OpenCL platform
   lib <- loadOpenCL (libraryOpenCL config)

   platforms <- clGetPlatformIDs lib

   (clMems, clLinks, clProcs) <- liftA (unzip3 . concat) $ forM platforms $ \platform -> do
      devices <- clGetDeviceIDs lib CL_DEVICE_TYPE_ALL platform
      context <- clCreateContext lib [CL_CONTEXT_PLATFORM platform] devices putStrLn
      forM devices $ \device -> do
         let queueProps = [CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, CL_QUEUE_PROFILING_ENABLE]
         queue <- clCreateCommandQueue lib context device queueProps
         let mem = CLMemory lib context device
         let link = CLLink lib queue HostMemory mem
         let proc = CLProcessor lib context queue device
         return (mem,link,proc)

   return $ Platform clMems clLinks clProcs

-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints (CLLink _ _ m1 m2) = (m1,m2)

-- | Get processor information string
procInfo :: Processor -> IO String
procInfo (CLProcessor lib _ _ dev) = do
  name <- clGetDeviceName lib dev
  vendor <- clGetDeviceVendor lib dev
  return $ "[OpenCL] " ++ name ++ " (" ++ vendor ++ ")"

procInfo HostProcessor = return "[Host]"


-- | Retrieve platform information string
platformInfo :: Platform -> IO String
platformInfo pf = do
  procs <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse procInfo (processors pf)
  mems <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse memInfo (memories pf)
  lks <- concatMap (\x -> "  - " ++ x ++ "\n") <$> traverse linkInfo (links pf)
  return ("Processors:\n" ++ procs ++ "Memories:\n" ++ mems ++ "Links:\n" ++ lks)


-- | Retrieve memory information string
memInfo :: Memory -> IO String
memInfo HostMemory = return "[Host] Host Memory"
memInfo (CLMemory lib _ dev) = do
  sz <- clGetDeviceGlobalMemSize lib dev
  devName <- clGetDeviceName lib dev
  return $ printf "[OpenCL] Memory %sBytes (%s)" (prettyShowSize (fromIntegral sz) Base) devName

-- | Retrieve memory name
memName :: Memory -> IO String
memName HostMemory = return "Host Memory"
memName (CLMemory lib _ dev) = do
   devName <- clGetDeviceName lib dev
   return $ printf "%s (OpenCL)" devName

-- | Retrieve link information string
linkInfo :: Link -> IO String
linkInfo (CLLink _ _ e1 e2) = do
   name1 <- memName e1
   name2 <- memName e2
   return $ printf "[OpenCL] Link between:\n      - %s\n      - %s" name1 name2
   

data Unit = Base | Kilo | Mega | Giga | Tera | Peta deriving (Show)

-- | Pretty print a size
prettyShowSize :: Double -> Unit -> String
prettyShowSize n unit = if n > 1023 then prettyShowSize (n / 1024.0) (nextUnit unit) else printf "%.2f %s" n (show unit)
   where
      nextUnit :: Unit -> Unit
      nextUnit u = case u of 
         Base -> Kilo
         Kilo -> Mega
         Mega -> Giga
         Giga -> Tera
         Tera -> Peta
         Peta -> undefined



-- | Retrieve memories attached to a given processor
processorMemories :: Processor -> [Memory]
processorMemories (CLProcessor lib ctx _ dev) = [CLMemory lib ctx dev]
processorMemories HostProcessor = [HostMemory]

