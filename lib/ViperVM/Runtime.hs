module ViperVM.Runtime (
  startRuntime, stopRuntime,
  mapVector,
  submitTask,
  waitForData,
  -- Events
  waitEvent,sync
  ) where

import ViperVM.Data
import ViperVM.Event
import ViperVM.Internals.StartStop
import ViperVM.Internals.Structures (Message(..), Runtime(..))
import ViperVM.KernelSet

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

-- | Map a Vector of host memory into runtime managed memory
-- You mustn't use mapped host memory
mapVector :: Runtime -> Primitive -> Word64 -> Ptr () -> IO (Event Data)
mapVector r prim sz ptr = sendRuntimeCmd r $ AppMapVector (VectorDesc prim sz) ptr

-- | Stop the given runtime
stopRuntime :: Runtime -> IO (Event ())
stopRuntime r = sendRuntimeCmd r AppQuit 

-- | Submit a task to the runtime system
submitTask :: Runtime -> KernelSet -> [Data] -> IO (Event [Data])
submitTask r ks ds = sendRuntimeCmd r $ AppTaskSubmit ks ds

-- | Wait for a data to be computed
waitForData :: Runtime -> Data -> IO (Event ())
waitForData r d = sendRuntimeCmd r $ AppWaitForData d