module ViperVM.Runtime.Platform (
   initFromPlatform
) where

import ViperVM.Runtime.Nodes
import qualified ViperVM.Platform as Pf
import qualified ViperVM.STM.TSet as TSet

import Control.Monad
import Control.Concurrent.STM
import Control.Applicative
import qualified Data.Map as Map
import Data.Map

-- | Initialize the runtime graph from a given Platform
initFromPlatform :: Pf.Platform -> STM (Memory,[Memory],[Processor],[Link])
initFromPlatform pf = do

   -- Create nodes
   mems <- forM (Pf.memories pf) initMemory
   procs <- forM (Pf.processors pf) initProcessor

   let memMap  = Map.fromList $ Pf.memories pf `zip` mems
       procMap = Map.fromList $ Pf.processors pf `zip` procs

   links <- forM (Pf.links pf) (`initLink` memMap)

   -- Create "proc <-> memory" edges
   forM_ (Pf.processors pf) $ \p ->
      forM_ (Pf.processorMemories p) $ \m -> do
         let proc = procMap ! p
             mem  = memMap ! m
         TSet.insert proc (memProcs mem)
         TSet.insert mem (procMemories proc)

   -- Create "mem <-> link" edges
   forM_ links $ \l -> do
      let src = linkSource l
          dst = linkTarget l
      TSet.insert l (memOutLinks src)
      TSet.insert l (memInLinks dst)

   let hostMem = memMap ! Pf.HostMemory

   return (hostMem,mems,procs,links)

-- | Initialize a memory node
initMemory :: Pf.Memory -> STM Memory
initMemory m = Memory m <$> TSet.empty <*> TSet.empty <*> TSet.empty <*> TSet.empty


-- | Initialize a processor
initProcessor :: Pf.Processor -> STM Processor
initProcessor p = Processor p <$> TSet.empty


-- | Initialize a link
initLink :: Pf.Link -> Map Pf.Memory Memory -> STM Link
initLink l memMap = do
   let (src,dst) = Pf.linkEndpoints l
       srcNode = memMap ! src
       dstNode = memMap ! dst
   return $ Link l srcNode dstNode
