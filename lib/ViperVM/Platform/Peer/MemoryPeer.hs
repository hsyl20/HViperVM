-- | Backend specific glue for memories and buffers
module ViperVM.Platform.Peer.MemoryPeer (
   MemoryPeer(..), memoryPeerApply,
   memoryName, memorySize,
   BufferPeer(..), bufferPeerApply,
   bufferSize, bufferAllocate, bufferRelease,
   hostBuffer, clBuffer
) where

import qualified ViperVM.Backends.Host.Memory as Host
import qualified ViperVM.Backends.OpenCL.Memory as CL
import qualified ViperVM.Backends.OpenCL.Buffer as CL
import qualified ViperVM.Backends.Host.Buffer as Host

import Data.Word
import Control.Applicative ( (<$>) )

-- | Backend memory
data MemoryPeer = HostMemory Host.Memory
                | CLMemory CL.Memory
                deriving (Eq,Ord,Show)

data BufferPeer = HostBuffer Host.Buffer
                | CLBuffer CL.Buffer

-- | Apply the appropriate given function to the peer memory
memoryPeerApply :: (Host.Memory -> a) -> (CL.Memory -> a) -> MemoryPeer -> a
memoryPeerApply fHost fCL m = case m of
   HostMemory peer -> fHost peer
   CLMemory peer   -> fCL peer

-- | Retrieve memory name
memoryName :: MemoryPeer -> String
memoryName = memoryPeerApply Host.memoryName CL.memoryName

-- | Retrieve memory size
memorySize :: MemoryPeer -> Word64
memorySize = memoryPeerApply Host.memorySize CL.memorySize

-- | Apply the appropriate given function to the peer buffer
bufferPeerApply :: (Host.Buffer -> a) -> (CL.Buffer -> a) -> BufferPeer -> a
bufferPeerApply fHost fCL b = case b of
   HostBuffer peer -> fHost peer
   CLBuffer peer -> fCL peer

-- | Return the size of the allocated buffer
bufferSize :: BufferPeer -> Word64
bufferSize = bufferPeerApply Host.bufferSize CL.bufferSize
              

-- | Try to allocate a buffer in a memory
bufferAllocate :: Word64 -> MemoryPeer -> IO (Maybe BufferPeer)
bufferAllocate sz m = memoryPeerApply 
   (\m' -> fmap HostBuffer <$> Host.bufferAllocate sz m')
   (\m' -> fmap CLBuffer <$> CL.bufferAllocate sz m')
   m

-- | Release a buffer
bufferRelease :: BufferPeer -> IO ()
bufferRelease = bufferPeerApply Host.bufferRelease CL.bufferRelease

hostBuffer :: BufferPeer -> Host.Buffer
hostBuffer (HostBuffer b) = b
hostBuffer _ = error "Invalid buffer type"

clBuffer :: BufferPeer -> CL.Buffer
clBuffer (CLBuffer b) = b
clBuffer _ = error "Invalid buffer type"
