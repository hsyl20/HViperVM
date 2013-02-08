module ViperVM.Runtime.Scheduler (
   createRuntime, 
   mapVector, mapVectorIO,
   submitTask, submitTaskIO,
   waitData, waitDataIO
) where

import Data.Word
import Foreign.Ptr
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import ViperVM.Platform
import ViperVM.Runtime.Data
import ViperVM.Runtime.Task

import ViperVM.KernelSet
import ViperVM.KernelInterface

import qualified ViperVM.STM.TSet as TSet

data Runtime = Runtime {
   platform :: Platform  -- ^ Platform the runtime is used on
}


-- | Create a runtime on a platform
createRuntime :: Platform -> IO Runtime
createRuntime pf = return (Runtime pf)


-- | Called when a data is mapped in host memory
notifyMapData :: Runtime -> Data -> STM ()
notifyMapData _ _ = return ()

-- | Called when a task is submitted
notifyTaskSubmit :: Runtime -> Task -> STM ()
notifyTaskSubmit _ _ = return ()


-- | IO version of mapVector
mapVectorIO :: Runtime -> Primitive -> Word64 -> Ptr () -> IO Data
mapVectorIO r prim n ptr = atomically $ mapVector r prim n ptr

-- | Map a vector in host memory
mapVector :: Runtime -> Primitive -> Word64 -> Ptr () -> STM Data
mapVector r prim n ptr = do
   let desc = VectorDesc prim n
       sz   = n * primitiveSize prim
       buf  = HostBuffer sz ptr
       reg  = Region1D 0 sz
       di   = Vector buf reg
   
   -- Create data
   dat <- Data <$> newTVar (Just desc) <*> TSet.singleton di <*> TSet.empty

   notifyMapData r dat

   return dat


-- | IO version of submitTask
submitTaskIO :: Runtime -> KernelSet -> [Data] -> IO [Data]
submitTaskIO r k params = atomically $ submitTask r k params

-- | Submit a task
submitTask :: Runtime -> KernelSet -> [Data] -> STM [Data]
submitTask r ks inData = do
   let ifa        = kernelInterface ks
       (inp,outp) = paramCount ifa

   when (length inData /= inp) $ error "Invalid number of parameters"

   -- Create output data
   outData <- forM [1..outp] $ const $ (Data <$> newTVar Nothing <*> TSet.empty <*> TSet.empty)

   let task = Task ks inData outData

   notifyTaskSubmit r task

   return outData


-- | IO version of waitData
waitDataIO :: Runtime -> [Data] -> IO ()
waitDataIO r ds = atomically $ waitData r ds

-- | Synchronously wait for some data to be computed
waitData :: Runtime -> [Data] -> STM ()
waitData _ ds = forM_ ds f
   where
      f d = do
         cond <- TSet.null (dataInstances d)
         when cond retry
