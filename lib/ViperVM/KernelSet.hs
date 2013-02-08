{-# LANGUAGE RecordWildCards #-}
module ViperVM.KernelSet where

import ViperVM.KernelInterface
import ViperVM.Runtime.Kernel

-- | A set of kernel performing the same operation and with the same interface
data KernelSet = KernelSet {
   kernelInterface :: KernelInterface,
   kernels :: [Kernel]
}
                 
instance Eq KernelSet where
  (==) (KernelSet _ a) (KernelSet _ b) = a == b

instance Ord KernelSet where
  compare (KernelSet _ a) (KernelSet _ b) = compare a b

instance Show KernelSet where
  show (KernelSet (KernelInterface {..}) _) = name
