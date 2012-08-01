module ViperVM.Task where

import ViperVM.Data
import ViperVM.KernelSet

data TaskParameter = TPReadOnly Data
                   | TPReadWrite Data Data
                   | TPAllocate Data

-- | A task
data Task = Task {
  kernelSet :: KernelSet,
  params :: [TaskParameter]
}

instance Show Task where
  show (Task ks _) = show ks

-- | Return data associated to a parameter
paramToData :: TaskParameter -> Data
paramToData (TPReadOnly d) = d
paramToData (TPReadWrite _ d) = d
paramToData (TPAllocate d) = d
