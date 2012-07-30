module ViperVM.Runtime (
  startRuntime, stopRuntime, registerKernel, 
  mapVector,createVector,
  submitTask,
  -- Events
  waitEvent,sync
  ) where

import ViperVM.Data
import ViperVM.Event
import ViperVM.Kernel
import ViperVM.RuntimeInternal
import ViperVM.Task

import Control.Concurrent
import Data.Word
import Foreign.Ptr

-- | Send a command to the runtime and get an asynchronous response
sendRuntimeCmd :: Runtime -> (Event a -> Message) -> IO (Event a)
sendRuntimeCmd (Runtime ch) msg = do
  v <- newEvent
  writeChan ch (msg v)
  return v

-- | Execute synchronously a function returning an event
sync :: IO (Event a) -> IO a
sync f = waitEvent =<< f

-- | Register a kernel in the runtime system
registerKernel :: Runtime -> Kernel -> IO (Event [Maybe CompiledKernel])
registerKernel r k = sendRuntimeCmd r $ RegisterKernel k

-- | Map a Vector of host memory into runtime managed memory
-- You mustn't use mapped host memory
mapVector :: Runtime -> Primitive -> Word64 -> Ptr () -> IO (Event Data)
mapVector r prim sz ptr = sendRuntimeCmd r $ MapVector (VectorDesc prim sz) ptr

-- | Create an uninitialized vector data
createVector :: Runtime -> Primitive -> Word64 -> IO (Event Data)
createVector r prim sz = sendRuntimeCmd r $ CreateVector (VectorDesc prim sz)

-- | Stop the given runtime
stopRuntime :: Runtime -> IO (Event ())
stopRuntime r = sendRuntimeCmd r Quit 

-- | Submit a task to the runtime system
submitTask :: Runtime -> Task -> IO (Event ())
submitTask r task = sendRuntimeCmd r $ SubmitTask task
