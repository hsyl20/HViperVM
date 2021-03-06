module ViperVM.Backends.OpenCL.Buffer (
   Buffer(..), bufferName, bufferAllocate, bufferRelease
) where

import ViperVM.Backends.Common.Buffer
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Memory

import Data.Word
import Foreign.Ptr
import Control.Monad (void)

data Buffer = Buffer {
   bufferLibrary :: OpenCLLibrary,
   bufferMemory  :: Memory,
   bufferPeer    :: CLMem,
   bufferSize    :: Word64
} deriving (Eq,Ord)

bufferName :: Buffer -> String
bufferName _ = "OpenCL Buffer"

-- | Try to allocate a buffer in a memory
bufferAllocate :: Word64 -> Memory -> IO (AllocResult Buffer)
bufferAllocate sz mem = do

   let 
      lib = memoryLibrary mem
      ctx = memoryContext mem

   clmem <- clCreateBuffer lib ctx [] (sz,nullPtr)
   -- TODO: Force allocation on dev (clMemMigrate if available, copybuffer otherwise)
   return $ AllocSuccess (Buffer lib mem clmem sz)

bufferRelease :: Buffer -> IO ()
bufferRelease b = do
   void $ clReleaseMemObject (bufferLibrary b) (bufferPeer b)
