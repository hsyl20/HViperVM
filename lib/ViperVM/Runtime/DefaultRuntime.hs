module ViperVM.Runtime.DefaultRuntime (
   createDefaultRuntime
) where

import ViperVM.Runtime.Nodes
import qualified ViperVM.Platform as Pf
import qualified ViperVM.STM.TSet as TSet

import Control.Monad
import Control.Concurrent.STM
import Control.Applicative
import qualified Data.Map as Map
import Data.Map

-- | Create a runtime on a platform
createDefaultRuntime :: Pf.Platform -> IO Runtime
createDefaultRuntime pf = do
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


-- | Initialize the runtime graph from a given Platform
initFromPlatform :: Pf.Platform -> STM (Memory,[Memory],[Processor],[Link])
initFromPlatform pf = do

   -- Create nodes
   mems <- forM (Pf.memories pf) initMemory
   procs <- forM (Pf.processors pf) initProcessor

   let memMap  = Map.fromList $ Pf.memories pf `zip` mems
       procMap = Map.fromList $ Pf.processors pf `zip` procs

   lnks <- forM (Pf.links pf) (`initLink` memMap)

   -- Create "proc <-> memory" edges
   forM_ (Pf.processors pf) $ \p ->
      forM_ (Pf.processorMemories p) $ \m -> do
         let proc = procMap ! p
             mem  = memMap ! m
         TSet.insert proc (memProcs mem)
         TSet.insert mem (procMemories proc)

   -- Create "mem <-> link" edges
   forM_ lnks $ \l -> do
      let src = linkSource l
          dst = linkTarget l
      TSet.insert l (memOutLinks src)
      TSet.insert l (memInLinks dst)

   let hostMem = memMap ! Pf.HostMemory

   return (hostMem,mems,procs,lnks)

-- | Initialize a memory node
initMemory :: Pf.Memory -> STM Memory
initMemory m = Memory m <$> TSet.empty <*> TSet.empty <*> TSet.empty <*> TSet.empty


-- | Initialize a processor
initProcessor :: Pf.Processor -> STM Processor
initProcessor p = Processor p <$> TSet.empty <*> TSet.empty


-- | Initialize a link
initLink :: Pf.Link -> Map Pf.Memory Memory -> STM Link
initLink l memMap = do
   let (src,dst) = Pf.linkEndpoints l
       srcNode = memMap ! src
       dstNode = memMap ! dst
   return $ Link l srcNode dstNode
