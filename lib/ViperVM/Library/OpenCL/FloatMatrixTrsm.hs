module ViperVM.Library.OpenCL.FloatMatrixTrsm where

import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Kernel
import ViperVM.Common.Util (roundTo)
import Paths_ViperVM

program :: IO Program
program = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixTrsm.cl"
   src <- readFile fileName
   initProgram src

kernel :: IO Kernel
kernel = do
   prog <- program
   initKernel prog "floatMatrixTrsm" [] config

-- | Configure kernel execution
config :: [CLKernelParameter] -> KernelConfiguration
config pms = KernelConfiguration gDim lDim pms
   where
      [CLUIntParam valid,_,_,_,_,_,_,_,_,_,_] = pms
      gDim = [1, roundTo 128 valid,1]
      lDim = [16,128,1]

