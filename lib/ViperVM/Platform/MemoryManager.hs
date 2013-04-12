module ViperVM.Platform.MemoryManager where

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

data MemoryManager = MemoryManager {
                        buffers :: Map Memory (TSet Buffer)
                     }

-- | Initialize a new memory manager
createMemoryManager :: Platform -> IO MemoryManager
createMemoryManager pf = do
   let mems = memories pf
   bufs <- fromList . (mems `zip`) <$> (atomically $ forM mems (\_ -> TSet.empty))
      
   return $ MemoryManager bufs


-- | Allocate a buffer in a memory
allocateBuffer :: MemoryManager -> Memory -> Word64 -> IO (Maybe Buffer)
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
releaseBuffer :: MemoryManager -> Buffer -> IO ()
releaseBuffer mm b = do
   let m = getBufferMemory b
       bufs = buffers mm ! m
   atomically $ TSet.delete b bufs
   Buffer.release b

-- | Retrieved allocated buffers in a memory
memoryBuffers :: MemoryManager -> Memory -> STM (Set Buffer)
memoryBuffers mm m = readTVar $ (buffers mm ! m)
