module ViperVM.Backends.Host.Buffer (
   Buffer,
   bufferSize, bufferPtr, bufferMemory,
   bufferAllocate, bufferRelease
) where

import ViperVM.Backends.Host.Memory

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word


data Buffer = Buffer {
   bufferSize :: Word64,
   bufferPtr :: Ptr (),
   bufferMemory :: Memory
} deriving (Eq,Ord)

bufferAllocate :: Memory -> Word64 -> IO (Maybe Buffer)
bufferAllocate m sz = do
   ptr <- mallocBytes (fromIntegral sz)
   return $ if ptr == nullPtr 
      then Nothing 
      else Just (Buffer sz ptr m)

bufferRelease :: Buffer -> IO ()
bufferRelease = free . bufferPtr
