module ViperVM.Scheduling.DataManager (
  dataManagerScheduler
  ) where

import ViperVM.RuntimeInternal
import ViperVM.Internals.Structures
import ViperVM.Internals.Logging
import ViperVM.Platform
import ViperVM.View
import ViperVM.Data

import Text.Printf


dataManagerScheduler :: Scheduler 

dataManagerScheduler (AppMapVector desc@(VectorDesc prim n) ptr r) = do
  let sz = n * primitiveSize prim
  logInfoR $ printf "Mapping %s vector of %d elements at address %s" (show prim) n (show ptr)
  buf <- mapHostBufferR sz ptr
  registerBuffer HostMemory buf
  let view = View1D buf 0 sz
  let di = Vector view
  d <- newData desc
  registerDataInstance d di
  setEventR r d

dataManagerScheduler _ = voidR
