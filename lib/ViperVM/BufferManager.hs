module ViperVM.BufferManager (
   BufferManager,
   ViperVM.BufferManager.init,
   buffers,
   allocate,
   mapHostBuffer,
   ViperVM.BufferManager.free,
   bufferMemory
) where

import Data.Map.Lazy
import qualified Data.Map.Lazy as Map
import Data.Set
import qualified Data.Set as Set
import Data.Maybe
import Data.Word

import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Applicative
import Control.Monad

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Backends.OpenCL

-- | Manage buffers allocated in each memory
newtype BufferManager = BufferManager (Map Memory (Set Buffer))

-- | Initialize a memory manager
init :: BufferManager
init = BufferManager Map.empty

-- | Retrieve buffers in a given memory
buffers :: BufferManager -> Memory -> Set Buffer
buffers (BufferManager m) mem = fromMaybe Set.empty (Map.lookup mem m)

-- | Associate a buffer to a memory in the buffer manager
associate :: BufferManager -> Memory -> Buffer -> BufferManager
associate (BufferManager m) mem buffer = BufferManager $ Map.update (wrap . Set.insert buffer) mem m
   where
      wrap s = if Set.null s then Nothing else Just s

-- | Dissociate a buffer to a memory in the buffer manager
dissociate :: BufferManager -> Memory -> Buffer -> BufferManager
dissociate (BufferManager m) mem buffer = BufferManager $ Map.update (wrap . Set.delete buffer) mem m
   where
      wrap s = if Set.null s then Nothing else Just s

-- | Allocate a buffer
allocate :: BufferManager -> Memory -> Word64 -> IO (BufferManager, Maybe Buffer)
allocate manager mem sz = do

   maybeBuffer <- case mem of

      HostMemory -> do 
         ptr <- mallocBytes (fromIntegral sz)
         return $ if ptr == nullPtr then Nothing else Just $ HostBuffer sz ptr

      CLMemory lib ctx _ -> do
         clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
         -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
         return $ Just $ CLBuffer lib mem clmem

   let newManager = fromMaybe manager (associate manager mem <$> maybeBuffer)

   return (newManager, maybeBuffer)

-- | Map host memory into a buffer
mapHostBuffer :: BufferManager -> Word64 -> Ptr () -> (BufferManager, Buffer)
mapHostBuffer manager sz ptr = (associate manager HostMemory buffer, buffer)
   where 
      buffer = HostBuffer sz ptr

-- | Release a buffer
free :: BufferManager -> Buffer -> IO BufferManager
free manager buffer = do
   
   case buffer of
      HostBuffer _ ptr -> Foreign.Marshal.Alloc.free ptr
      CLBuffer lib _ m -> void $ clReleaseMemObject lib m

   return (dissociate manager (bufferMemory manager buffer) buffer)

-- | Return memory containing the buffer
bufferMemory :: BufferManager -> Buffer -> Memory
bufferMemory _ (HostBuffer {}) = HostMemory
bufferMemory _ (CLBuffer _ mem _) = mem
