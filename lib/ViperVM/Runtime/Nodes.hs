{-# LANGUAGE RecordWildCards #-}

-- | Node of the runtime graph
module ViperVM.Runtime.Nodes where


import Control.Concurrent.STM
import ViperVM.STM.TSet
import ViperVM.Runtime.Data

import qualified ViperVM.Platform as Pf

-- | A physical memory
data Memory = Memory {
   peerMem :: Pf.Memory,
   memDataInstances :: TSet DataInstance
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

-- | A data
data Data = Data {
   dataDesc :: TVar (Maybe DataDesc),
   dataInstances :: TSet DataInstance,
   dataTransfers :: TSet Transfer
}

-- | A data transfer
data Transfer = Transfer {
   transferLink :: Link,
   transferSource :: DataInstance,
   transferTarget :: DataInstance,
   transferData :: Data
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

-- | A set of kernel performing the same operation and with the same interface
data KernelSet = KernelSet {
   kernelInterface :: KernelInterface,
   kernels :: [Pf.Kernel]
}
                 
instance Eq KernelSet where
  (==) (KernelSet _ a) (KernelSet _ b) = a == b

instance Ord KernelSet where
  compare (KernelSet _ a) (KernelSet _ b) = compare a b

instance Show KernelSet where
  show (KernelSet (KernelInterface {..}) _) = name


data KernelParameter = KPReadOnly Data  -- ^ Access a data in read-only mode
               | KPReadWrite Data -- ^ Access the first data in virtual read-write mode. The second data is the result and the first is left unmodified
               | KPAllocate DataDesc  -- ^ Allocate a new data

data KernelInterface = KernelInterface {
  name :: String,                                  -- ^ Kernel identifier (name)
  paramCount :: (Int,Int),                         -- ^ Number of parameters (in, out)
  makeParameters :: [Data] -> [KernelParameter],   -- ^ Create kernel parameters from input data
  makeResult :: [Data] -> [Data]                   -- ^ Filter data to return as kernel results
}
