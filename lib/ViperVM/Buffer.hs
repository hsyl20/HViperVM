module ViperVM.Buffer where

import ViperVM.Platform
import ViperVM.Backends.OpenCL.Types

import Data.Word
import Foreign.Ptr
import ViperVM.Backends.OpenCL


type BufferId = Word
data Buffer = CLBuffer OpenCLLibrary Memory CLMem | HostBuffer Word64 (Ptr ())

instance Show Buffer where
  show (CLBuffer {}) = "OpenCL Buffer"
  show (HostBuffer {}) = "Host Buffer"
              
getCLBuffer :: Buffer -> CLMem
getCLBuffer buf = clmem
   where
      CLBuffer _ _ clmem = buf
