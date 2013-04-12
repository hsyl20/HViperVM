module ViperVM.Platform.Buffer where

import ViperVM.Platform.Memory
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Monad


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

-- | Return memory into which the buffer is allocated
getBufferMemory :: Buffer -> Memory
getBufferMemory (CLBuffer _ mem _) = mem
getBufferMemory (HostBuffer {}) = HostMemory


-- | Ttry to allocate a buffer in a memory
allocate :: Memory -> Word64 -> IO (Maybe Buffer)
allocate mem sz = case mem of

      HostMemory -> do 
         ptr <- mallocBytes (fromIntegral sz)
         return $ if ptr == nullPtr then Nothing else Just $ HostBuffer sz ptr

      CLMemory lib ctx _ -> do
         clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
         -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
         return $ Just $ CLBuffer lib mem clmem

-- | Release a buffer
release :: Buffer -> IO ()
release buffer = 
   case buffer of
      HostBuffer _ ptr -> Foreign.Marshal.Alloc.free ptr
      CLBuffer lib _ m -> void $ clReleaseMemObject lib m
