{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines Buffer entities
--
-- Buffers are memory spaces that have been allocated in a memory
--
module ViperVM.Platform.Buffer (
        Buffer(..),
        bufferSize, bufferMemory, 
        allocate, release
) where

import ViperVM.Platform.Memory
import qualified ViperVM.Backends.OpenCL.Buffer as CL
import qualified ViperVM.Backends.Host.Buffer as Host

import Data.Word
import Data.Typeable
import Control.Applicative ( (<$>) )

-- | A buffer allocated in a memory
data Buffer = 
     CLBuffer CL.Buffer
   | HostBuffer Host.Buffer
   deriving (Ord,Eq,Typeable)

instance Show Buffer where
  show (CLBuffer b) = CL.bufferName b
  show (HostBuffer {}) = "Host Buffer"

-- | Return the size of the allocated buffer
bufferSize :: Buffer -> Word64
bufferSize (CLBuffer b) = CL.bufferSize b
bufferSize (HostBuffer b) = Host.bufferSize b
              
-- | Return memory into which the buffer is allocated
bufferMemory :: Buffer -> Memory
bufferMemory (CLBuffer b) = CLMemory (CL.bufferMemory b)
bufferMemory (HostBuffer {}) = HostMemory

-- | Ttry to allocate a buffer in a memory
allocate :: Memory -> Word64 -> IO (Maybe Buffer)
allocate (CLMemory mem) sz = fmap CLBuffer <$> CL.bufferAllocate mem sz
allocate HostMemory sz = fmap HostBuffer <$> Host.bufferAllocate HostMemory sz



-- | Release a buffer
release :: Buffer -> IO ()
release (HostBuffer b) = Host.bufferRelease b
release (CLBuffer b) = CL.bufferRelease b
