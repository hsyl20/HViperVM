{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines Buffer entities
--
-- Buffers are memory spaces that have been allocated in a memory
--
module ViperVM.Platform.Buffer (
        Buffer(..),
        getBufferSize, getBufferMemory, 
        isCLBuffer, isHostBuffer,
        getCLBuffer, getHostBufferPtr,
        allocate, release
) where

import ViperVM.Platform.Memory
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL

import Data.Word
import Data.Typeable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Monad

-- | A buffer allocated in a memory
data Buffer = 
   CLBuffer OpenCLLibrary Memory CLMem Word64 |
   HostBuffer Word64 (Ptr ())
   deriving (Ord,Eq,Typeable)

instance Show Buffer where
  show (CLBuffer {}) = "OpenCL Buffer"
  show (HostBuffer {}) = "Host Buffer"

-- | Return the size of the allocated buffer
getBufferSize :: Buffer -> Word64
getBufferSize (CLBuffer _ _ _ sz) = sz
getBufferSize (HostBuffer sz _) = sz
              
-- | Return memory into which the buffer is allocated
getBufferMemory :: Buffer -> Memory
getBufferMemory (CLBuffer _ mem _ _) = mem
getBufferMemory (HostBuffer {}) = HostMemory

-- | Indicates if it is an OpenCL buffer
isCLBuffer :: Buffer -> Bool
isCLBuffer (CLBuffer {}) = True
isCLBuffer _ = False

-- | Indicates if it is an host buffer
isHostBuffer :: Buffer -> Bool
isHostBuffer (HostBuffer {}) = True
isHostBuffer _ = False

-- | Return CLMem entity associated to an OpenCL buffer
getCLBuffer :: Buffer -> CLMem
getCLBuffer (CLBuffer _ _ m _) = m
getCLBuffer _ = error "Cannot use getCLBuffer on a non OpenCL buffer"

-- | Return pointer associated to a host buffer
getHostBufferPtr :: Buffer -> Ptr ()
getHostBufferPtr (HostBuffer _ ptr) = ptr
getHostBufferPtr _ = error "Cannot use getHostBufferPtr on a non host buffer"

-- | Ttry to allocate a buffer in a memory
allocate :: Memory -> Word64 -> IO (Maybe Buffer)
allocate mem sz = case mem of

      HostMemory -> do 
         ptr <- mallocBytes (fromIntegral sz)
         return $ if ptr == nullPtr then Nothing else Just $ HostBuffer sz ptr

      CLMemory lib ctx _ -> do
         clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
         -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
         return $ Just $ CLBuffer lib mem clmem sz

-- | Release a buffer
release :: Buffer -> IO ()
release buffer = 
   case buffer of
      HostBuffer _ ptr -> Foreign.Marshal.Alloc.free ptr
      CLBuffer lib _ m _ -> void $ clReleaseMemObject lib m
