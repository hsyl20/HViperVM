module ViperVM.Backends.Host.Buffer (
   Buffer(..),
   bufferAllocate,
   bufferRelease
) where

import ViperVM.Platform.Memory

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word


data Buffer = Buffer {
   bufferSize :: Word64,
   bufferPtr :: Ptr ()
} deriving (Eq,Ord)

bufferAllocate :: Memory -> Word64 -> IO (Maybe Buffer)
bufferAllocate _ sz = do
   ptr <- mallocBytes (fromIntegral sz)
   return $ if ptr == nullPtr 
      then Nothing 
      else Just (Buffer sz ptr)

bufferRelease :: Buffer -> IO ()
bufferRelease = free . bufferPtr
