{-# LANGUAGE RecordWildCards, TupleSections #-}

{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Kernel as CL
import ViperVM.Platform.Processor
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.KernelExecutionResult
import ViperVM.Platform.ProcessorCapabilities

import Control.Applicative ( (<$>) )

-- | A kernel
data Kernel = CLKernel CL.Kernel

instance Show Kernel where
   show (CLKernel k) = show k

-- | Processors for which the kernel is compiled
kernelCompiledFor :: Kernel -> IO [Processor]
kernelCompiledFor (CLKernel k) = fmap CLProcessor <$> CL.kernelCompiledFor k

kernelIsCompiledFor :: Kernel -> Processor -> IO Bool
kernelIsCompiledFor (CLKernel k) (CLProcessor proc) = CL.kernelIsCompiledFor k proc

kernelEnsureCompiledFor :: Kernel -> Processor -> IO ()
kernelEnsureCompiledFor (CLKernel k) (CLProcessor proc) = CL.kernelEnsureCompiledFor k proc

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Processor -> Bool
supportConstraints cs p = all (`supportConstraint` p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Processor -> Bool
supportConstraint DoublePrecisionSupport proc = procSupports proc DoubleFloatingPoint

-- | Indicate if a processor can execute a given kernel
canExecute :: Processor -> Kernel -> Bool
canExecute p (CLKernel k)  = supportConstraints (CL.kernelConstraints k) p

-- | Try to compile kernel for the given processors
compile :: Kernel -> [Processor] -> IO ()
compile (CLKernel k) ps = CL.kernelCompile k (fmap (\(CLProcessor p) -> p) ps)

-- | Execute a kernel on a given processor synchronously
execute :: Processor -> Kernel -> [KernelParameter] -> IO ExecutionResult
execute (CLProcessor p) (CLKernel k) params = CL.kernelExecute p k params
