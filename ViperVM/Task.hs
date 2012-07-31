module ViperVM.Task where

import ViperVM.Data
import ViperVM.Event
import ViperVM.KernelSet

-- | A task
data Task = Task KernelSet [Data] [Event ()] (Event ())

instance Show Task where
  show (Task ks _ _ _) = show ks
