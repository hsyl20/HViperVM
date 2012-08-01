module ViperVM.Task where

import ViperVM.Data
import ViperVM.KernelSet

-- | A task
data Task = Task {
  kernelSet :: KernelSet,
  inputData :: [Data],
  kernelData :: [Data]
}

instance Show Task where
  show (Task ks _ _) = show ks
