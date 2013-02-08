module ViperVM.Platform.Buffer where

import ViperVM.Platform.Memory
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL

import Data.Word
import Foreign.Ptr


data Buffer = 
   CLBuffer OpenCLLibrary Memory CLMem |
   HostBuffer Word64 (Ptr ())
   deriving (Ord,Eq)

instance Show Buffer where
  show (CLBuffer {}) = "OpenCL Buffer"
  show (HostBuffer {}) = "Host Buffer"
              
getCLBuffer :: Buffer -> CLMem
getCLBuffer buf = clmem
   where
      CLBuffer _ _ clmem = buf
