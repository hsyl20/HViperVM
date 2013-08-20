module ViperVM.Library.OpenCL.FloatMatrixPotrf where

import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Kernel
import Paths_ViperVM

program :: IO Program
program = do
   fileName <- getDataFileName "lib/ViperVM/Library/OpenCL/FloatMatrixPotrf.cl"
   src <- readFile fileName
   initProgram src

kernel :: IO Kernel
kernel = do
   prog <- program
   initKernel prog "floatMatrixPotrf" [] config

-- | Configure kernel execution
config :: [CLKernelParameter] -> KernelConfiguration
config pms = KernelConfiguration gDim lDim pms
   where
      gDim = [16,16,1]
      lDim = [16,16,1]

