module ViperVM.Platform.Memory (
   Memory(..), memoryName, memoryInfo
) where

import qualified ViperVM.Backends.Host.Memory as Host
import qualified ViperVM.Backends.OpenCL.Memory as CL
import Text.Printf
import Data.Word

-- | A memory node
data Memory = HostMemory Host.Memory
            | CLMemory CL.Memory
            deriving (Eq,Ord,Show)

data Unit = Base | Kilo | Mega | Giga | Tera | Peta 

instance Show Unit where
   show Base = "bytes"
   show Kilo = "kB"
   show Mega = "MB"
   show Giga = "GB"
   show Tera = "TB"
   show Peta = "PB"

-- | Retrieve memory information string
memoryInfo :: Memory -> String
memoryInfo m = printf "%s - %s" (memoryName m) (prettyMemorySize m)

-- | Retrieve memory name
memoryName :: Memory -> String
memoryName (HostMemory m) = Host.memoryName m
memoryName (CLMemory m) = CL.memoryName m

-- | Retrieve memory size
memorySize :: Memory -> Word64
memorySize (HostMemory m) = Host.memorySize m
memorySize (CLMemory m) = CL.memorySize m

-- | Pretty print memory size
prettyMemorySize :: Memory -> String
prettyMemorySize m = prettyShowSize (fromIntegral (memorySize m)) Base

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
