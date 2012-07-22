module ViperVM.Runtime (
  startRuntime, 
  stopRuntime, stopRuntimeSync,
  registerKernel, registerKernelSync,
  mapVector, mapVectorSync,
  -- Events
  waitEvent,
  ) where

import ViperVM.RuntimeInternal

import ViperVM.Data
import ViperVM.Kernel
import ViperVM.Event

import Foreign.Ptr
import Control.Concurrent

-- | Send a command to the runtime and get an asynchronous response
sendRuntimeCmd :: Runtime -> (Event a -> Message) -> IO (Event a)
sendRuntimeCmd (Runtime ch) msg = do
  v <- newEvent
  writeChan ch (msg v)
  return v

-- | Register a kernel in the runtime system
registerKernel :: Runtime -> Kernel -> IO (Event [Maybe CompiledKernel])
registerKernel r k = sendRuntimeCmd r $ RegisterKernel k

-- | Synchronous version of registerKernel
registerKernelSync :: Runtime -> Kernel -> IO [Maybe CompiledKernel]
registerKernelSync r k = waitEvent =<< registerKernel r k

-- | Map a Vector of host memory into runtime managed memory
-- You mustn't use mapped host memory
mapVector :: Runtime -> VectorDesc -> Ptr () -> IO (Event Data)
mapVector r desc ptr = sendRuntimeCmd r $ MapVector desc ptr

-- | Synchronous version of mapVector
mapVectorSync :: Runtime -> VectorDesc -> Ptr () -> IO Data
mapVectorSync r desc ptr = waitEvent =<< mapVector r desc ptr

-- | Stop the given runtime
stopRuntime :: Runtime -> IO (Event ())
stopRuntime r = sendRuntimeCmd r $ Quit 

-- | Synchronous version of stopRuntime
stopRuntimeSync :: Runtime -> IO ()
stopRuntimeSync r = waitEvent =<< stopRuntime r
