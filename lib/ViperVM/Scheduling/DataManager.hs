module ViperVM.Scheduling.DataManager (
  dataManagerScheduler
  ) where


import ViperVM.Data
import ViperVM.Internals.Logging
import ViperVM.Internals.Memory
import ViperVM.Internals.Structures
import ViperVM.Platform
import ViperVM.RuntimeInternal
import ViperVM.Task
import ViperVM.Region

import Data.Foldable (traverse_)
import Text.Printf


dataManagerScheduler :: Scheduler 

dataManagerScheduler (AppMapVector desc@(VectorDesc prim n) ptr r) = do
  let sz = n * primitiveSize prim
  logInfoR $ printf "Mapping %s vector of %d elements at address %s" (show prim) n (show ptr)
  buf <- mapHostBufferR sz ptr
  registerBufferR HostMemory buf
  let region = Region1D buf 0 sz
  let di = Vector region
  d <- newData desc
  registerDataInstanceR d di
  setEventR r d


-- Check that the data has not already been computed
-- and trigger or store the data event accordingly
dataManagerScheduler (AppWaitForData d ev) = do
  cond <- dataInstanceExistsR d
  if cond
    then setEventR ev ()
    else registerDataEventR d ev
    

-- Indicate data that have been computed upon task completion
dataManagerScheduler (TaskComplete (Task _ params)) = do
  flip traverse_ (outputDatas params) $ \d -> do
    postMessageR (DataComputed d)
    evs <- getDataEventsR d
    traverse_ (flip setEventR ()) evs


dataManagerScheduler _ = voidR
