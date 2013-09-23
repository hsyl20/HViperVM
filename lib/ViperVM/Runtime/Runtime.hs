{-# LANGUAGE LambdaCase #-}
module ViperVM.Runtime.Runtime (
   Runtime,
   initRuntime, allocate, release, releaseMany,
   peekFloatMatrix, pokeFloatMatrix, pokeDummyFloatMatrix,
   execute, platform, scheduler,
   loadBuiltin, loadBuiltins, MakeBuiltin, readData, dataBuiltin,
   allocateLinked, unsplit,
   customLog, debugLog, errorLog
) where

import ViperVM.Common.Logger
import ViperVM.Platform (Platform,hostMemories)
import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins
import ViperVM.Platform.Scheduler
import ViperVM.Platform.KernelManager
import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.SharedObjectManager hiding (objectManager)
import ViperVM.Platform.SharedObject

import Data.Foldable (traverse_)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Dynamic
import Data.Map as Map
import Data.Traversable (traverse)

data Runtime = Runtime {
   platform :: Platform,
   scheduler :: Scheduler,
   schedChan :: TChan SchedMsg
}

-- | Initialize a runtime system
initRuntime :: Platform -> Scheduler -> Logger -> IO Runtime
initRuntime pf sch lg = do
   chan <- newBroadcastTChanIO
   initScheduler sch som km =<< atomically (dupTChan chan)
   return $ Runtime pf lg om som sch chan

-- | Allocate and initialize a matrix of floats
pokeFloatMatrix :: Runtime -> Descriptor -> [[Float]] -> IO SharedObject
pokeFloatMatrix rt desc ds = pokeFloatMatrix' rt desc (Just ds)

pokeFloatMatrix' :: Runtime -> Descriptor -> Maybe [[Float]] -> IO SharedObject
pokeFloatMatrix' rt desc ds = do
   let 
      om = objectManager rt
      (MatrixDesc prim w h) = desc

   so <- allocate rt desc
   
   let hostMem = head . hostMemories $ platform rt
   Just o <- allocateMatrixObject om hostMem prim w h 0

   case ds of
      Just ds' -> pokeHostFloatMatrix om o ds'
      Nothing -> return ()

   atomically (attachInstance so o)

   return so


pokeDummyFloatMatrix :: Runtime -> Descriptor -> IO SharedObject
pokeDummyFloatMatrix r desc = pokeFloatMatrix' r desc Nothing


-- | Retrieve values of a matrix of floats
peekFloatMatrix :: Runtime -> SharedObject -> IO [[Float]]
peekFloatMatrix rt so = do
   let 
      som = sharedObjectManager rt
      om = objectManager rt

   let hostMem = head . hostMemories $ platform rt
   obj <- ensureInMemory som hostMem so
   peekHostFloatMatrix om obj


-- | Send a message to the scheduler and wait for its answer
sendSchedMsg :: Runtime -> (TSchedResult -> SchedMsg) -> IO SchedResult
sendSchedMsg rt msg = do
   result <- atomically $ do
      r <- newTVar SchedNoResult
      writeTChan (schedChan rt) (msg r)
      return r
  
   r' <- atomically $ do
      r <- readTVar result
      when (r == SchedNoResult) retry
      return r
   return r'

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


unsplit :: Runtime -> [[SharedObject]] -> IO SharedObject
unsplit rt = unsplitSharedObject (sharedObjectManager rt)



-- | Put custom message in log
customLog :: Runtime -> String -> IO ()
customLog rt s = logger rt (CustomLog s)

-- | Put error message in log
errorLog :: Runtime -> String -> IO ()
errorLog rt s = logger rt (ErrorLog s)

-- | Put debug message in log
debugLog :: Runtime -> String -> IO ()
debugLog rt s = logger rt (DebugLog s)
