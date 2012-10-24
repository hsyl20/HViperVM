module ViperVM.KernelSet where

import ViperVM.KernelInterface
import ViperVM.Kernel

-- | A set of kernel performing the same operation and with the same interface
data KernelSet = KernelSet KernelInterface [Kernel]
                 
instance Eq KernelSet where
  (==) (KernelSet _ a) (KernelSet _ b) = a == b

instance Ord KernelSet where
  compare (KernelSet _ a) (KernelSet _ b) = compare a b

instance Show KernelSet where
  show (KernelSet (KernelInterface n _ _) _) = n
