{-# LANGUAGE RecordWildCards #-}

-- | Node of the runtime graph
module ViperVM.Runtime.Nodes where


import Control.Concurrent.STM
import ViperVM.STM.TSet
import ViperVM.Runtime.Data

import qualified ViperVM.Platform as Pf

-- | A physical memory
data Memory = Memory {
   memPeer :: Pf.Memory,
   memDataInstances :: TSet DataInstance,
   memProcs :: TSet Processor,
   memOutLinks :: TSet Link,
   memInLinks :: TSet Link
}

instance Eq Memory where
   (==) m1 m2 = (==) (memPeer m1) (memPeer m2)

instance Ord Memory where
   compare m1 m2 = compare (memPeer m1) (memPeer m2)

-- | A processing unit
data Processor = Processor {
   procPeer :: Pf.Processor,
   procMemories :: TSet Memory
} 

instance Eq Processor where
   (==) p1 p2 = (==) (procPeer p1) (procPeer p2)

instance Ord Processor where
   compare p1 p2 = compare (procPeer p1) (procPeer p2)

-- | A link between two memories
data Link = Link {
   linkPeer :: Pf.Link,
   linkSource :: Memory,
   linkTarget :: Memory
}

instance Eq Link where
   (==) p1 p2 = (==) (linkPeer p1) (linkPeer p2)

instance Ord Link where
   compare p1 p2 = compare (linkPeer p1) (linkPeer p2)

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
