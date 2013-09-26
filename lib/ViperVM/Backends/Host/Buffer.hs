module ViperVM.Backends.Host.Buffer (
   Buffer,
   bufferSize, bufferPtr, bufferMemory,
   bufferAllocate, bufferRelease
) where

import ViperVM.Backends.Common.Buffer
import ViperVM.Backends.Host.Memory

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word


data Buffer = Buffer {
   bufferSize :: Word64,
   bufferPtr :: Ptr (),
   bufferMemory :: Memory
} deriving (Eq,Ord)

bufferAllocate :: Word64 -> Memory -> IO (AllocResult Buffer)
bufferAllocate sz m = do
   ptr <- mallocBytes (fromIntegral sz)
   return $ if ptr == nullPtr 
      then AllocError
      else AllocSuccess (Buffer sz ptr m)

bufferRelease :: Buffer -> IO ()
bufferRelease = free . bufferPtr
