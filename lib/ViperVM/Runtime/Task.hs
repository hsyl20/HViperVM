module ViperVM.Runtime.Task where

import ViperVM.Runtime.Data
import ViperVM.KernelSet

-- | A task
data Task = Task KernelSet [Data] [Data]

instance Show Task where
  show (Task ks _ _) = show ks
