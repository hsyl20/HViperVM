module ViperVM.Runtime (
   Runtime, RNode(..) 
) where

import ViperVM.Graph
import ViperVM.Data
import ViperVM.Task
import ViperVM.STM.TIntSet
import qualified Data.IntSet as IntSet
import qualified ViperVM.Platform as Pf
import ViperVM.Platform.Memory
import ViperVM.Platform.Processor
import ViperVM.Platform.Link
import ViperVM.Platform (Platform)

import Control.Concurrent.STM
import Control.Monad
import Control.Applicative ( (<$>), (<*>))

-- | Contain the whole state of the runtime system using STM
data Runtime = Runtime {
      -- Event channel
      chan :: TChan RuntimeEvent,
      -- Whole graph
      graph :: Graph RNode,
      -- Indexes
      processors :: TNodeSet,
      memories :: TNodeSet,
      links :: TNodeSet
   }

data RuntimeEvent = 
   MemoryNodeAdded Node |
   ProcessorAdded Node  |
   LinkAdded Node

-- | A node of the runtime graph
data RNode =
   -- | A physical memory
   MemoryNode Memory |
   -- | A processing unit
   ProcessorNode {
      proc :: Processor ,
      procMemories :: TNodeSet
   } |
   -- | A link between two memories
   LinkNode {
      link :: Link,
      linkSource :: Node,
      linkTarget :: Node
   } |
   -- | A data
   DataNode {
      dataDesc :: DataDesc,
      dataInstances :: TNodeSet,
      dataTransfers :: TNodeSet
   } |
   -- | Data partitioning operator
   Partition {
      splitData :: Node
   } |
   -- | An instance of a data in a memory
   DataInstanceNode {
      dataInstance :: DataInstance
   } |
   -- | A data transfer
   DataTransferNode {
      transferLink :: Node,
      transferSource :: Node,
      transferTarget :: Node,
      transferData :: Node
   } |
   -- | A task
   TaskNode {
      task :: Task,
      taskInstances :: TNodeSet,
      taskCandidateProcs :: TNodeSet
   } |
   -- | An instance of a task
   TaskInstanceNode {
      taskInstanceTask :: Node,
      taskInstanceParams :: TNodeSet,
      taskInstanceProc :: Node
   }

-- | Create a runtime on the given platform
createRuntime :: Platform -> IO Runtime
createRuntime pf = atomically $ do

   
   r <- Runtime <$> newTChan <*> newGraph <*> empty <*> empty <*> empty
   
   forM_ (Pf.memories pf) (addMemory r)
   forM_ (Pf.processors pf) (addProcessor r)
   forM_ (Pf.links pf) (addLink r)

   return r

-- Find a node in a set using a predicate on its value
findNode :: Runtime -> (RNode -> Bool) -> NodeSet -> STM (Maybe Node)
findNode r f = g . IntSet.elems
   where
      g :: [Int] -> STM (Maybe Node)
      g [] = return Nothing
      g (x:xs) = do
         v <- nodeValue (graph r) x
         if f v then return (Just x) else g xs
         
         

-- | Add a memory in the runtime
addMemory :: Runtime -> Memory -> STM Node
addMemory r m = do
   n <- addNode (graph r) (MemoryNode m) =<< readTVar =<< empty
   insert n (memories r)
   return n

-- Find a memory node
findMemory :: Runtime -> Memory -> STM (Maybe Node)
findMemory r m = findNode r (== MemoryNode m) =<< (memories r)
      

-- | Add a processor in the runtime
addProcessor :: Runtime -> Processor -> STM Node
addProcessor r p = do
   let mems = processorMemories p
   n <- addNode (graph r) (ProcessorNode p mems) =<< readTVar =<< empty
   insert n (processors r)
   return n

-- | Add a link in the runtime
addLink :: Runtime -> Link -> STM Node
addLink r l = do
   n <- addNode (graph r) (LinkNode l) =<< readTVar =<< empty
   insert n (links r)
   return n
