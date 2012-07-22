module ViperVM.Scheduling.Default (
  defaultScheduler
  ) where

import ViperVM.Platform
import ViperVM.Buffer
import ViperVM.Data
import ViperVM.View
import ViperVM.Runtime

import Control.Monad.State
import Control.Concurrent
import Text.Printf

defaultScheduler :: Scheduler

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

defaultScheduler _ = undefined
