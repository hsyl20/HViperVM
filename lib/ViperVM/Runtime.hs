module ViperVM.Runtime (
   Runtime, RNode(..) 
) where

import ViperVM.Graph
import ViperVM.Data
import ViperVM.Task
import ViperVM.STM.TIntSet
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
      -- Whole graph
      graph :: Graph RNode,
      -- Indexes
      processors :: TNodeSet,
      memories :: TNodeSet
   }

-- | A node of the runtime graph
data RNode =
   MemoryNode Memory |
   ProcessorNode Processor |
   LinkNode Link |
   DataNode {
      desc :: DataDesc,
      dataInstances :: TNodeSet,
      transfers :: TNodeSet
   } |
   DataInstanceNode DataInstance |
   DataTransferNode {
      link :: Node,
      source :: Node,
      target :: Node,
      parentData :: Node
   } |
   TaskNode {
      task :: Task,
      taskInstances :: TNodeSet,
      candidates :: TNodeSet
   } |
   TaskInstanceNode {
      parentTask :: Node,
      params :: TNodeSet,
      processor :: Node
   }

createRuntime :: Platform -> IO Runtime
createRuntime pf = atomically $ do

   g <- newGraph
   r <- Runtime g <$> empty <*> empty
   
   forM_ (Pf.processors pf) (addProcessor r)
   forM_ (Pf.memories pf) (addMemory r)
--   forM (Pf.links pf) (addLink r)

   return r


-- | Add a processor in the runtime
addProcessor :: Runtime -> Processor -> STM Node
addProcessor r p = do
   n <- addNode (graph r) (ProcessorNode p) =<< readTVar =<< empty
   insert n (processors r)
   return n

-- | Add a processor in the runtime
addMemory :: Runtime -> Memory -> STM Node
addMemory r m = do
   n <- addNode (graph r) (MemoryNode m) =<< readTVar =<< empty
   insert n (memories r)
   return n
