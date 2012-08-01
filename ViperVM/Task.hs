module ViperVM.Task where

import ViperVM.Data
import ViperVM.Event
import ViperVM.KernelSet

-- | A task
data Task = Task KernelSet [Data]

instance Show Task where
  show (Task ks _) = show ks
