module ViperVM.Buffer where

import ViperVM.Platform
import ViperVM.Backends.OpenCL.Types

import Data.Word
import Control.Monad ( void )
import Foreign.Marshal.Alloc
import Foreign.Ptr
import ViperVM.Backends.OpenCL

data Buffer = CLBuffer OpenCLLibrary Memory CLMem | HostBuffer Word64 (Ptr ())

instance Eq Buffer where
  (==) (CLBuffer _ mem1 m1) (CLBuffer _ mem2 m2) = (mem1 == mem2) && (m1 == m2)
  (==) (HostBuffer _ m1) (HostBuffer _ m2) = m1 == m2
  (==) _ _ = False


-- | Allocate a buffer in the given memory
allocBuffer :: Memory -> Word64 -> IO Buffer
allocBuffer HostMemory sz = do
  ptr <- mallocBytes (fromIntegral sz)
  return $ HostBuffer sz ptr

allocBuffer mem@(CLMemory lib ctx dev) sz = do
  clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
  -- TODO: Force allocation on dev
  return $ CLBuffer lib mem clmem


-- | Release a buffer
freeBuffer :: Buffer -> IO ()
freeBuffer (HostBuffer _ ptr) = free ptr
freeBuffer (CLBuffer lib _ m) = void $ clReleaseMemObject lib m

-- | Return memory containing the buffer
getBufferMemory :: Buffer -> Memory
getBufferMemory (HostBuffer _ _) = HostMemory
getBufferMemory (CLBuffer _ mem _) = mem
