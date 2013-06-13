-- | A buffer manager maintains the set of allocated buffers in each memory
--
module ViperVM.Platform.BufferManager (
   BufferManager, createBufferManager, releaseBufferManager,
   allocateBuffer, releaseBuffer,
   getMemoryBuffers, getBufferManagerPlatform
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Buffer as Buffer
import ViperVM.Platform.Platform

import ViperVM.STM.TSet as TSet

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Data.Map
import Data.Word
import Data.Set (Set)
import Data.Traversable (forM)
import Data.Foldable (forM_)

-- | A buffer manager
--
-- Use 'allocateBuffer' and 'releaseBuffer' instead of
-- 'ViperVM.Platform.Buffer.allocate' and 'ViperVM.Platform.Buffer.release'
-- otherwise buffers will not be known by the buffer manager
data BufferManager = BufferManager {
                        platform :: Platform,
                        buffers :: Map Memory (TSet Buffer)
                     }

-- | Return associated platform
getBufferManagerPlatform :: BufferManager -> Platform
getBufferManagerPlatform = platform

-- | Initialize a new buffer manager
createBufferManager :: Platform -> IO BufferManager
createBufferManager pf = do
   let mems = memories pf
   bufs <- fromList . (mems `zip`) <$> (atomically $ forM mems (\_ -> TSet.empty))
      
   return $ BufferManager pf bufs

-- | Release a buffer manager
releaseBufferManager :: BufferManager -> IO ()
releaseBufferManager _ = return ()

-- | Allocate a buffer in a memory
allocateBuffer :: BufferManager -> Memory -> Word64 -> IO (Maybe Buffer)
allocateBuffer mm mem sz = do
   buf <- Buffer.allocate mem sz
   forM_ buf insertBuffer
   return buf
   where
      insertBuffer buf = do
         atomically $ TSet.insert buf bufs
         return buf
      bufs = buffers mm ! mem

-- | Release a buffer
releaseBuffer :: BufferManager -> Buffer -> IO ()
releaseBuffer mm b = do
   let m = getBufferMemory b
       bufs = buffers mm ! m
   atomically $ TSet.delete b bufs
   Buffer.release b

-- | Retrieved allocated buffers in a memory
getMemoryBuffers :: BufferManager -> Memory -> STM (Set Buffer)
getMemoryBuffers mm m = readTVar $ (buffers mm ! m)
