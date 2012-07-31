module ViperVM.KernelSet where

import ViperVM.KernelInterface
import ViperVM.Kernel

-- | A set of kernel performing the same operation and with the same interface
data KernelSet = KernelSet KernelInterface [Kernel]

instance Show KernelSet where
  show (KernelSet (KernelInterface n _) _) = n
