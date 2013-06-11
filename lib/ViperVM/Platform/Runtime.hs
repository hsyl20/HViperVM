{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Runtime (
      Runtime,
      initRuntime, allocate, release, releaseMany,
      peekFloatMatrix, pokeFloatMatrix,
      execute, platform, scheduler,
      loadBuiltin, loadBuiltins, MakeBuiltin, readData, dataBuiltin
   ) where

import ViperVM.Platform (Platform)
import ViperVM.Platform.Memory
import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins
import ViperVM.Platform.Scheduler
import ViperVM.Platform.KernelManager
import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.SharedObjectManager (SharedObjectManager,createSharedObjectManager,ensureInMemory)
import ViperVM.Platform.SharedObject

import Data.Foldable (traverse_)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Dynamic
import Data.Map as Map
import Data.Traversable (traverse)

data Runtime = Runtime {
      platform :: Platform,
      objectManager :: ObjectManager,
      sharedObjectManager :: SharedObjectManager,
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
   
   initScheduler sch som km =<< atomically(dupTChan chan)

   return $ Runtime pf om som sch chan

-- | Allocate a new data
allocate :: Runtime -> Descriptor -> IO SharedObject
allocate _ desc = do
   so <- atomically (createSharedObject desc)
   return so


-- | Release data
release :: Runtime -> SharedObject -> IO ()
release _ _ = return () -- TODO

-- | Release several data
releaseMany :: Runtime -> [SharedObject] -> IO ()
releaseMany rt = traverse_ (release rt)

-- | Allocate and initialize a matrix of floats
pokeFloatMatrix :: Runtime -> Descriptor -> [[Float]] -> IO SharedObject
pokeFloatMatrix rt desc ds = do
   let 
      om = objectManager rt
      (MatrixDesc prim w h) = desc

   so <- allocate rt desc
   
   Just o <- allocateMatrixObject om HostMemory prim w h 0

   pokeHostFloatMatrix om o ds

   atomically (attachObject so o)

   return so



-- | Retrieve values of a matrix of floats
peekFloatMatrix :: Runtime -> SharedObject -> IO [[Float]]
peekFloatMatrix rt so = do
   let 
      som = sharedObjectManager rt
      om = objectManager rt

   obj <- ensureInMemory som HostMemory so
   peekHostFloatMatrix om obj


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

type MakeBuiltin = (Expr -> SharedObject) -> (SharedObject -> Expr) -> (ObjectKernel -> [SharedObject] -> IO ()) -> (Descriptor -> IO SharedObject) -> IO Builtin

-- | Initialize builtins
loadBuiltin :: Runtime -> MakeBuiltin -> IO Builtin
loadBuiltin rt make = make readData writeData exec alloc
   where
      exec = execute rt
      alloc = allocate rt
      writeData = Data . toDyn

loadBuiltins :: Runtime -> [(String,MakeBuiltin)] -> IO (Map String Builtin)
loadBuiltins rt ds = traverse (loadBuiltin rt) (Map.fromList ds)

readData :: Expr -> SharedObject
readData (Data u) = fromDyn u (error "Invalid data")
readData _ = error "Invalid data parameter"


dataBuiltin :: Typeable a => a -> MakeBuiltin
dataBuiltin v _ _ _ _ = return (Builtin [] . const . return . Data $ toDyn v)
