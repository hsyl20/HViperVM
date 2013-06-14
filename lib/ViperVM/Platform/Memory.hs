module ViperVM.Platform.Memory (
   Memory(..), memName, memInfo
) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query
import System.IO.Unsafe
import Text.Printf

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

data Unit = Base | Kilo | Mega | Giga | Tera | Peta deriving (Show)

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
