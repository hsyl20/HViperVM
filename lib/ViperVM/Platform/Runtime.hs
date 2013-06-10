{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Runtime (
      initRuntime, allocate, release, releaseMany,
      initFloatMatrix,
      execute
   ) where

import ViperVM.Platform (Platform, Configuration, initPlatform)
import ViperVM.Platform.Memory
import ViperVM.Platform.Scheduler
import ViperVM.Platform.KernelManager
import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.SharedObjectManager (SharedObjectManager,createSharedObjectManager)
import ViperVM.Platform.SharedObject

import Data.Foldable (traverse_)
import Control.Concurrent.STM
import Control.Monad (when)

data Runtime = Runtime {
      platform :: Platform,
      bufferManager :: BufferManager,
      regionManager :: RegionLockManager,
      kernelManager :: KernelManager,
      transferManager :: RegionTransferManager,
      objectManager :: ObjectManager,
      shareObjectManager :: SharedObjectManager,
      scheduler :: Scheduler,
      schedChan :: TChan SchedMsg
   }

-- | Initialize a runtime system
initRuntime :: Platform -> Scheduler -> IO Runtime
initRuntime pf sch = do
   bm <- createBufferManager pf
   rm <- createRegionLockManager bm
   km <- createKernelManager rm
   tm <- createRegionTransferManager rm
   om <- createObjectManager tm km
   som <- createSharedObjectManager om
   chan <- newBroadcastTChanIO
   sched <- initScheduler sch som km =<< atomically(dupTChan chan)

   return $ Runtime pf bm rm km tm om som sch chan

-- | Allocate a new data
allocate :: Runtime -> Descriptor -> IO SharedObject
allocate rt desc = do
   so <- atomically (createSharedObject desc)
   return so


-- | Release data
release :: Runtime -> SharedObject -> IO ()
release rt so = return ()

-- | Release several data
releaseMany :: Runtime -> [SharedObject] -> IO ()
releaseMany rt = traverse_ (release rt)

-- | Allocate and initialize a matrix of floats
initFloatMatrix :: Runtime -> Descriptor -> [[Float]] -> IO SharedObject
initFloatMatrix rt desc ds = do
   let 
      om = objectManager rt
      (MatrixDesc prim w h) = desc

   so <- allocate rt desc
   
   Just o <- allocateMatrixObject om HostMemory prim w h 0

   pokeHostFloatMatrix om o ds

   atomically (attachObject so o)

   return so


-- | Send a message to the scheduler and wait for its answer
sendSchedMsg :: Runtime -> (TSchedResult -> SchedMsg) -> IO SchedResult
sendSchedMsg rt msg = do
   result <- atomically $ do
      r <- newTVar SchedNoResult
      writeTChan (schedChan rt) (msg r)
      return r
  
   atomically $ do
      r <- readTVar result
      when (r == SchedNoResult) retry
      return r

-- | Execute the given kernel
execute :: Runtime -> ObjectKernel -> [SharedObject] -> IO ()
execute rt k args = do
   sendSchedMsg rt (SchedExec k args) >>= \case
      SchedSuccess -> return ()
      SchedError -> error "Error during scheduling"
      SchedNoResult -> error "Scheduler returned no result"
