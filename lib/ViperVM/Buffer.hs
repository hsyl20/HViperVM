module ViperVM.Buffer where

import ViperVM.Platform
import ViperVM.Backends.OpenCL.Types

import Data.Word
import Control.Monad ( void )
import Foreign.Marshal.Alloc
import Foreign.Ptr
import ViperVM.Backends.OpenCL


data Buffer = Buffer BufferId BufferImpl

type BufferId = Word
data BufferImpl = CLBuffer OpenCLLibrary Memory CLMem | HostBuffer Word64 (Ptr ())

instance Eq Buffer where
  (==) (Buffer a _) (Buffer b _) = a == b

instance Ord Buffer where
  compare (Buffer a _) (Buffer b _) = compare a b

instance Show Buffer where
  show (Buffer i _) = "Buffer " ++ (show i)
              
allocBuffer :: Memory -> Word64 -> IO BufferImpl
allocBuffer HostMemory sz = do
  ptr <- mallocBytes (fromIntegral sz)
  return $ HostBuffer sz ptr

allocBuffer mem@(CLMemory lib ctx dev) sz = do
  clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
  -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
  return $ CLBuffer lib mem clmem

-- | Release a buffer
freeBuffer :: BufferImpl -> IO ()
freeBuffer (HostBuffer _ ptr) = free ptr
freeBuffer (CLBuffer lib _ m) = void $ clReleaseMemObject lib m

-- | Return memory containing the buffer
getBufferImplMemory :: BufferImpl -> Memory
getBufferImplMemory (HostBuffer {}) = HostMemory
getBufferImplMemory (CLBuffer _ mem _) = mem

-- | Return buffer implementation associated to a given buffer
getBufferImpl :: Buffer -> BufferImpl
getBufferImpl (Buffer _ impl) = impl

getBufferMemory :: Buffer -> Memory
getBufferMemory = getBufferImplMemory . getBufferImpl

