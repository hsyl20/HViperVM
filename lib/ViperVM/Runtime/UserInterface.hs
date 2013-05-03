module ViperVM.Runtime.UserInterface (
   mapVector, mapVectorIO,
   submitTask, submitTaskIO,
   waitData, waitDataIO
) where

import Data.Word
import Foreign.Ptr
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import qualified ViperVM.STM.TSet as TSet
import qualified ViperVM.Platform as Pf
import qualified ViperVM.Platform.Primitive as Prim
import ViperVM.Runtime.Nodes
import ViperVM.Runtime.Data
import ViperVM.Runtime.Kernel

-- | IO version of mapVector
mapVectorIO :: Runtime -> Pf.Primitive -> Word64 -> Ptr () -> IO Data
mapVectorIO r prim n ptr = atomically $ mapVector r prim n ptr

-- | Map a vector in host memory
mapVector :: Runtime -> Pf.Primitive -> Word64 -> Ptr () -> STM Data
mapVector r prim n ptr = do
   let desc = Pf.VectorDesc prim n
       sz   = n * Prim.size prim
       buf  = Pf.HostBuffer sz ptr
       reg  = Pf.Region1D 0 sz

   dat <- createData r (Just desc) 
   di <- createDataInstance (hostMemory r) [(buf,reg)]
   attachDataInstance dat di

   writeTChan (events r) $ NotifyMapData dat

   return dat

   


-- | IO version of submitTask
submitTaskIO :: Runtime -> KernelInterface -> [Data] -> IO [Data]
submitTaskIO r k params = atomically $ submitTask r k params

-- | Submit a task
submitTask :: Runtime -> KernelInterface -> [Data] -> STM [Data]
submitTask r ki inData = do
   let (inp,outp) = paramCount ki

   when (length inData /= inp) $ error "Invalid number of parameters"

   -- Create output data
   outData <- forM [1..outp] $ const (createData r Nothing)
   
   mk <- fetchMetaKernel r ki 

   task <- Task mk inData outData <$> TSet.empty

   writeTChan (events r) $ NotifyTaskSubmit task

   return outData




-- | IO version of waitData
waitDataIO :: Runtime -> [Data] -> IO ()
waitDataIO r ds = atomically $ waitData r ds

-- | Synchronously wait for some data to be computed
waitData :: Runtime -> [Data] -> STM ()
waitData r ds = writeTChan (events r) (NotifyWaitData ds) >> forM_ ds f
   where
      f d = do
         cond <- TSet.null (dataInstances d)
         when cond retry
