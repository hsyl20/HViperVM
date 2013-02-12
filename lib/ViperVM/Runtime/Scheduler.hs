module ViperVM.Runtime.Scheduler (
   createRuntime, 
   mapVector, mapVectorIO,
   submitTask, submitTaskIO,
   waitData, waitDataIO
) where

import Data.Word
import Data.Map
import qualified Data.Map as Map
import Foreign.Ptr
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import qualified ViperVM.STM.TSet as TSet
import qualified ViperVM.Platform as Pf
import ViperVM.Runtime


data Runtime = Runtime {
   processors :: [Processor],
   memories :: [Memory],
   kernels :: TVar (Map KernelSet MetaKernel),
   hostMemory :: Memory,
   links :: [Link],
   lastDataId :: TVar Int,
   notifyMapData :: Data -> STM (),
   notifyTaskSubmit :: Task -> STM (),
   notifyWaitData :: [Data] -> STM ()
}


-- | Create a runtime on a platform
createRuntime :: Pf.Platform -> IO Runtime
createRuntime pf = do
   (hostMem,mems,procs,lnks) <- atomically $ initFromPlatform pf

   lstDataId <- atomically $ newTVar 0
   kernlSet <- atomically $ newTVar Map.empty

   let r = Runtime {
         processors = procs,
         memories = mems,
         kernels = kernlSet,
         hostMemory = hostMem,
         links = lnks,
         lastDataId = lstDataId,
         notifyMapData = \_ -> return (),
         notifyTaskSubmit = \_ -> return (),
         notifyWaitData = \_ -> return ()
      }

   return r


-- | Create a new data
createData :: Runtime -> Maybe Pf.DataDesc -> STM Data
createData r desc = do
   lstId <- readTVar (lastDataId r)
   writeTVar (lastDataId r) (lstId+1)  -- FIXME: potential overflow
   Data lstId <$> newTVar desc <*> TSet.empty <*> TSet.empty
   

-- | IO version of mapVector
mapVectorIO :: Runtime -> Pf.Primitive -> Word64 -> Ptr () -> IO Data
mapVectorIO r prim n ptr = atomically $ mapVector r prim n ptr

-- | Map a vector in host memory
mapVector :: Runtime -> Pf.Primitive -> Word64 -> Ptr () -> STM Data
mapVector r prim n ptr = do
   let desc = Pf.VectorDesc prim n
       sz   = n * Pf.primitiveSize prim
       buf  = Pf.HostBuffer sz ptr
       reg  = Pf.Region1D 0 sz

   dat <- createData r (Just desc) 
   di <- createDataInstance (hostMemory r) [(buf,reg)]
   attachDataInstance dat di

   notifyMapData r dat

   return dat



-- Find or create MetaKernel node
fetchKernelNode :: Runtime -> KernelSet -> STM MetaKernel
fetchKernelNode r ks = do
   kernelMap <- readTVar (kernels r)

   case Map.lookup ks kernelMap of
      Nothing -> do
         mk <- MetaKernel ks <$> newTVar (Map.empty)
         writeTVar (kernels r) (Map.insert ks mk kernelMap)
         return mk
      Just mk -> return mk
   


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
   outData <- forM [1..outp] $ const (createData r Nothing)

   mk <- fetchKernelNode r ks

   task <- Task mk inData outData <$> TSet.empty

   notifyTaskSubmit r task

   return outData




-- | IO version of waitData
waitDataIO :: Runtime -> [Data] -> IO ()
waitDataIO r ds = atomically $ waitData r ds

-- | Synchronously wait for some data to be computed
waitData :: Runtime -> [Data] -> STM ()
waitData r ds = notifyWaitData r ds >> forM_ ds f
   where
      f d = do
         cond <- TSet.null (dataInstances d)
         when cond retry
