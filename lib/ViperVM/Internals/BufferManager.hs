module ViperVM.Internals.BufferManager (
   init,
   buffers,
   allocate,
   free
) where

import Data.Map.Lazy
import qualified Data.Map.Lazy as Map
import Data.Set
import qualified Data.Set as Set
import Data.Maybe
import Data.Word

import Foreign.Ptr
import qualified Foreign.Ptr as Ptr
import Foreign.Marshal.Alloc

import Control.Applicative

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Backends.OpenCL

-- | Manage buffers allocated in each memory
data BufferManager = BufferManager (Map Memory (Set Buffer))

-- | Initialize a memory manager
init :: [Memory] -> BufferManager
init mems = BufferManager Map.empty

-- | Retrieve buffers in a given memory
buffers :: BufferManager -> Memory -> Set Buffer
buffers (BufferManager m) mem = fromMaybe Set.empty (Map.lookup mem m)

-- | Associate a buffer to a memory in the buffer manager
associate :: BufferManager -> Memory -> Buffer -> BufferManager
associate manager mem buffer = BufferManager $ Set.insert buffer (buffers manager mem)

-- | Dissociate a buffer to a memory in the buffer manager
dissociate :: BufferManager -> Memory -> Buffer -> BufferManager
dissociate manager mem buffer = BufferManager $ Set.remove buffer (buffers manager mem)

-- | Allocate a buffer
allocate :: BufferManager -> Memory -> Word64 -> IO (BufferManager, Maybe Buffer)
allocate manager mem sz = do

   maybeBuffer <- case mem of

      HostMemory -> do 
         ptr <- mallocBytes (fromIntegral sz)
         if ptr == nullPtr then Nothing else Just $ HostBuffer sz ptr

      mem@(CLMemory lib ctx dev) -> do
         clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
         -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
         Just $ CLBuffer lib mem clmem

   let newManager = fromMaybe manager (associate manager mem <$> maybeBuffer)

   return (newManager, maybeBuffer)

-- | Release a buffer
free :: BufferManager -> Buffer -> IO BufferManager
free manager buffer = do
   
   let mem = memory buffer

   case mem of
      HostBuffer _ ptr -> Ptr.free ptr
      CLBuffer lib _ m -> void $ clReleaseMemObject lib m

   return (dissociate manager mem buffer)

-- | Return memory containing the buffer
memory :: Buffer -> Memory
memory (HostBuffer {}) = HostMemory
memory (CLBuffer _ mem _) = mem
