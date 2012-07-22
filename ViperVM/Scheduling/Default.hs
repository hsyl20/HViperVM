module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Data
import ViperVM.View
import ViperVM.Event
import ViperVM.RuntimeInternal

import Control.Monad.State
import Control.Concurrent
import Text.Printf

defaultScheduler :: Scheduler

defaultScheduler (Quit ev) = do
  logInfo "Stopping the runtime..."
  logInfo "This log will now be closed"
  shutdownLogger
  lift $ setEvent ev ()

defaultScheduler (MapVector desc@(VectorDesc prim n) ptr r) = do
  let sz = n * primitiveSize prim
  logInfo $ printf "Mapping %s vector of %d elements at address %s" (show prim) n (show ptr)
  buf <- lift . return $ HostBuffer sz ptr
  registerBuffer HostMemory buf
  let view = View1D buf 0 sz
  let di = Vector desc view
  d <- newData
  registerDataInstance d di
  lift $ putMVar r d

defaultScheduler (RegisterKernel k v) = do
  ck <- compileKernelR k
  lift $ putMVar v ck
  return ()

defaultScheduler _ = undefined
