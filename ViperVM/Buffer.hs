module ViperVM.Buffer where

import ViperVM.Platform
import ViperVM.Backends.OpenCL.Types

import Data.Word
import Control.Monad ( void )
import Foreign.Marshal.Alloc
import Foreign.Ptr
import ViperVM.Backends.OpenCL

type BufferID = Word

data Buffer = CLBuffer BufferID OpenCLLibrary Memory CLMem | HostBuffer BufferID Word64 (Ptr ())
              
instance Eq Buffer where
  (==) a b = getBufferID a == getBufferID b

instance Ord Buffer where
  compare a b = compare (getBufferID a) (getBufferID b)

instance Show Buffer where
  show b@(CLBuffer {}) = "OpenCL Buffer " ++ (show $ getBufferID b)
  show b@(HostBuffer {}) = "Host Buffer " ++ (show $ getBufferID b)

allocBuffer :: BufferID -> Memory -> Word64 -> IO Buffer
allocBuffer i HostMemory sz = do
  ptr <- mallocBytes (fromIntegral sz)
  return $ HostBuffer i sz ptr

allocBuffer i mem@(CLMemory lib ctx dev) sz = do
  clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
  -- TODO: Force allocation on dev
  return $ CLBuffer i lib mem clmem

-- | Return buffer unique identifier
getBufferID :: Buffer -> BufferID
getBufferID (CLBuffer i _ _ _) = i
getBufferID (HostBuffer i _ _) = i

-- | Release a buffer
freeBuffer :: Buffer -> IO ()
freeBuffer (HostBuffer _ _ ptr) = free ptr
freeBuffer (CLBuffer _ lib _ m) = void $ clReleaseMemObject lib m

-- | Return memory containing the buffer
getBufferMemory :: Buffer -> Memory
getBufferMemory (HostBuffer _ _ _) = HostMemory
getBufferMemory (CLBuffer _ _ mem _) = mem
