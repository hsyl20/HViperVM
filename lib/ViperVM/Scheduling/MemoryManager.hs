module ViperVM.Scheduling.MemoryManager (
  memoryManagerScheduler
  ) where


import ViperVM.Data
import ViperVM.Transfer
import ViperVM.Logging
import ViperVM.Structures

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
   desc <- descriptorR d
   let size = backingBufferSize desc
   buf <- allocateBufferR m size
   logInfoR $ printf "[Memory Manager] Try allocating a buffer of size %d for %s in %s" size (show d) (show m)

   case buf of
      
      Just buffer -> do
         let di = createDataInstance desc buffer
         associateDataInstanceR d di
         postMessageR $ DataAllocated d di

      Nothing -> do -- Allocation failed. We will retry later (FIXME: may loop forever)
         postMessageR RequestsStored

executeRequestR (RequestTransfer ms d) = do
   let m = head ms
   desc <- descriptorR d
   let size = backingBufferSize desc

   buf <- allocateBufferR m size
   logInfoR $ printf "[Memory Manager] Try allocating a buffer of size %d for %s in %s for a data transfer" size (show d) (show m)

   case buf of
      
      Just buffer -> do
         let di = createDataInstance desc buffer

         logInfoR $ printf "[Memory Manager] Try transfering a buffer of size %d for %s in %s" size (show d) (show m)
         -- Select source
         srcs <- instancesR d
         let src = head srcs
         -- Select link
         links <- getLinksBetweenDataInstances src di
         let link = head links
         let tr = transferDataInstance link src di
         logInfoR $ printf "[Memory Manager] Scheduling %s..." (show tr)
         lift $ performTransfer tr
         associateDataInstanceR d di
         postMessageR $ DataTransfered d di

      Nothing -> do -- Allocation failed. We will retry later (FIXME: may loop forever)
         postMessageR RequestsStored



executeRequestR _ = voidR
