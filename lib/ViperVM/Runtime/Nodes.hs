{-# LANGUAGE RecordWildCards #-}

-- | Node of the runtime graph
module ViperVM.Runtime.Nodes where


import Control.Concurrent.STM
import ViperVM.STM.TSet

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
   procMemories :: TSet Memory,
   procTaskInstances :: TSet TaskInstance
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
   dataId :: Int,                         -- id used for Eq and Ord (any better solution?)
   dataDesc :: TVar (Maybe Pf.DataDesc),
   dataInstances :: TSet DataInstance,
   dataTransfers :: TSet Transfer
} 

instance Eq Data where
   (==) d1 d2 = (dataId d1) == (dataId d2) 

instance Ord Data where
   compare d1 d2 = compare (dataId d1) (dataId d2)


-- | A data instance
data DataInstance = DataInstance {
   dataInstanceMem :: Memory,
   dataInstanceRegions :: [(Pf.Buffer,Pf.Region)],
   dataInstanceData :: TVar (Maybe Data),
   dataInstanceOutTransfers :: TSet Transfer,
   dataInstanceInTransfer :: TVar (Maybe Transfer)
}

instance Eq DataInstance where
   (==) p1 p2 = (==) (dataInstanceRegions p1) (dataInstanceRegions p2)

instance Ord DataInstance where
   compare p1 p2 = compare (dataInstanceRegions p1) (dataInstanceRegions p2)

-- | A data transfer
data Transfer = Transfer {
   transferLink :: Link,
   transferSource :: DataInstance,
   transferTarget :: DataInstance
} deriving (Eq,Ord)

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

instance Eq Task where
   (==) t1 t2 = (f t1) == (f t2)
      where
         f x = (kernelSet x, inputParams x, outputParams x)

instance Ord Task where
   compare t1 t2 = compare (f t1) (f t2)
      where
         f x = (kernelSet x, inputParams x, outputParams x)


-- | An instance of a task
data TaskInstance = TaskInstance {
   taskInstanceTask :: Task,
   taskInstanceParams :: [DataInstance],
   taskInstanceProc :: Processor
} deriving (Eq,Ord)

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
               | KPAllocate Pf.DataDesc  -- ^ Allocate a new data

data KernelInterface = KernelInterface {
  name :: String,                                  -- ^ Kernel identifier (name)
  paramCount :: (Int,Int),                         -- ^ Number of parameters (in, out)
  makeParameters :: [Data] -> [KernelParameter],   -- ^ Create kernel parameters from input data
  makeResult :: [Data] -> [Data]                   -- ^ Filter data to return as kernel results
}
