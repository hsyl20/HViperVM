module ViperVM.Scheduling.MemoryManager (
  memoryManagerScheduler
  ) where


import ViperVM.Data
import ViperVM.Transfer
import ViperVM.Internals.Logging
import ViperVM.Internals.Memory
import ViperVM.Internals.Structures

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Lens.Lazy
import qualified Data.Map as Map
import Text.Printf


memoryManagerScheduler :: Scheduler 

memoryManagerScheduler RequestsStored = do
  reqs <- gets (Map.keys . getL requestTasks)
  traverse_ executeRequestR reqs

memoryManagerScheduler _ = voidR


executeRequestR :: TaskRequest -> R ()
executeRequestR (RequestAllocation ms d) = do
  let m = head ms
  let size = backingBufferSize (dataDescriptor d)
  buf <- createBufferR m size
  di <- lift . return $ createDataInstance (dataDescriptor d) buf
  logInfoR $ printf "[Memory Manager] Allocate a buffer of size %d for %s in %s" size (show d) (show m)
  registerDataInstanceR d di
  postMessageR $ DataAllocated d di

executeRequestR (RequestTransfer ms d) = do
  let m = head ms
  let size = backingBufferSize (dataDescriptor d)
  buf <- createBufferR m size
  di <- lift . return $ createDataInstance (dataDescriptor d) buf
  logInfoR $ printf "[Memory Manager] Transfer a buffer of size %d for %s in %s" size (show d) (show m)
  -- Select source
  srcs <- getInstancesR d
  let src = head srcs
  -- Select link
  links <- getLinksBetweenDataInstances src di
  let link = head links
  let tr = transferDataInstance link src di
  logInfoR $ printf "[Memory Manager] Scheduling %s..." (show tr)
  lift $ performTransfer tr
  registerDataInstanceR d di
  postMessageR $ DataTransfered d di

executeRequestR _ = voidR
