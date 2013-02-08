module ViperVM.Runtime where

import ViperVM.STM.TSet
import ViperVM.Runtime.Data
import ViperVM.KernelSet
import qualified ViperVM.Platform as Pf

import Control.Concurrent.STM

data Runtime = Runtime {
   memories :: TSet Memory,
   processors :: TSet Processor
}

-- | A physical memory
data Memory = Memory {
   peerMem :: Pf.Memory
}

-- | A processing unit
data Processor = Processor {
   proc :: Pf.Processor,
   procMemories :: TVar [Memory]
}

-- | A link between two memories
data Link = Link {
   link :: Pf.Link,
   linkSource :: Memory,
   linkTarget :: Memory
}

-- | Data partitioning operator
data Partition = Partition {
   splitData :: Data
}

-- | A task
data Task = Task {
   kernelSet :: KernelSet,
   inputParams :: [Data],
   outputParams :: [Data],
   taskInstances :: TSet TaskInstance
}

-- | An instance of a task
data TaskInstance = TaskInstance {
   taskInstanceTask :: Task,
   taskInstanceParams :: TSet DataInstance,
   taskInstanceProc :: Processor
}
