module ViperVM.Platform.Memory (
   Memory(..), memoryName, memoryInfo
) where

import qualified ViperVM.Backends.OpenCL.Memory as CL
import Text.Printf

-- | A memory node
data Memory = HostMemory
            | CLMemory CL.Memory
            deriving (Eq,Ord,Show)

data Unit = Base | Kilo | Mega | Giga | Tera | Peta deriving (Show)

-- | Retrieve memory information string
memoryInfo :: Memory -> IO String
memoryInfo HostMemory = return "[Host] Host Memory"
memoryInfo (CLMemory m) = do
  sz <- CL.memoryGlobalSize m
  devName <- CL.memoryName m
  return $ printf "[OpenCL] Memory %sBytes (%s)" (prettyShowSize (fromIntegral sz) Base) devName

-- | Retrieve memory name
memoryName :: Memory -> IO String
memoryName HostMemory = return "Host Memory"
memoryName (CLMemory m) = CL.memoryName m

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
