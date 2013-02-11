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


-- | Ttry to allocate a buffer in a memory
allocateBuffer :: Memory -> Word64 -> IO (Maybe Buffer)
allocateBuffer mem sz = do

   case mem of

      HostMemory -> do 
         ptr <- mallocBytes (fromIntegral sz)
         return $ if ptr == nullPtr then Nothing else Just $ HostBuffer sz ptr

      CLMemory lib ctx _ -> do
         clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
         -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
         return $ Just $ CLBuffer lib mem clmem

-- | Release a buffer
releaseBuffer :: Buffer -> IO ()
releaseBuffer buffer = do
   case buffer of
      HostBuffer _ ptr -> Foreign.Marshal.Alloc.free ptr
      CLBuffer lib _ m -> void $ clReleaseMemObject lib m
