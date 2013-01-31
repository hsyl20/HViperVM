module ViperVM.Scheduling.DataManager (
  dataManagerScheduler
  ) where


import ViperVM.Data
import ViperVM.Logging
import ViperVM.Structures
import ViperVM.Task
import ViperVM.Region

import Data.Foldable (for_,traverse_)
import Text.Printf


dataManagerScheduler :: Scheduler 

dataManagerScheduler (AppMapVector desc@(VectorDesc prim n) ptr r) = do
  let sz = n * primitiveSize prim
  logInfoR $ printf "Mapping %s vector of %d elements at address %s" (show prim) n (show ptr)
  buf <- mapHostBufferR sz ptr
  let region = Region1D 0 sz
  let di = Vector buf region
  d <- allocateDataR desc
  associateDataInstanceR d di
  setEventR r d


-- Check that the data has not already been computed
-- and trigger or store the data event accordingly
dataManagerScheduler (AppWaitForData d ev) = do
  cond <- dataInstanceExistsR d
  if cond
    then setEventR ev ()
    else registerDataEventR d ev
    

-- Indicate data that have been computed upon task completion
dataManagerScheduler (TaskComplete (Task _ params)) = for_ (outputDatas params) $ \d -> do
    postMessageR (DataComputed d)
    evs <- getDataEventsR d
    traverse_ (`setEventR` ()) evs


dataManagerScheduler _ = voidR
