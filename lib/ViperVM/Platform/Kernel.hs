{-# LANGUAGE RecordWildCards, TupleSections, LambdaCase #-}

module ViperVM.Platform.Kernel where

import Control.Applicative ((<$>))

import ViperVM.Platform.Proc
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.ProcessorCapabilities

import qualified ViperVM.Platform.Peer.KernelPeer as Peer

-- | A kernel
data Kernel = Kernel {
   kernelPeer :: Peer.KernelPeer
} deriving (Ord,Eq)

instance Show Kernel where
   show k = show (kernelPeer k)

initKernelsIO :: [IO Peer.KernelPeer] -> IO [Kernel]
initKernelsIO peers = fmap Kernel <$> sequence peers

-- | Constraints of the kernel
kernelConstraints :: Kernel -> [KernelConstraint]
kernelConstraints = Peer.kernelConstraints . kernelPeer

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Proc -> Bool
supportConstraints cs p = all (`supportConstraint` p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Proc -> Bool
supportConstraint DoublePrecisionSupport proc = procSupports proc DoubleFloatingPoint

-- | Indicate if a processor can execute a given kernel
canExecute :: Proc -> Kernel -> Bool
canExecute p k  = supportConstraints (kernelConstraints k) p

-- | Execute a kernel on a given processor synchronously
execute :: Proc -> Kernel -> [KernelParameter] -> IO ()
execute p k params = do
   Peer.kernelExecute (kernelPeer k) params (procPeer p)
