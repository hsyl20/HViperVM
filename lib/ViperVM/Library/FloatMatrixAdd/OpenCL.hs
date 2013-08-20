module ViperVM.Library.FloatMatrixAdd.OpenCL where

import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Kernel
import ViperVM.Common.Util (roundTo)
import Paths_ViperVM

program :: IO Program
program = do
   fileName <- getDataFileName "lib/ViperVM/Library/FloatMatrixAdd/OpenCL.cl"
   src <- readFile fileName
   initProgram src

kernel :: IO Kernel
kernel = do
   prog <- program
   initKernel prog "floatMatrixAdd" [] config

-- | Configure kernel execution
config :: [CLKernelParameter] -> KernelConfiguration
config pms = KernelConfiguration gDim lDim pms
   where
      [CLUIntParam width, CLUIntParam height,_,_,_,_,_,_,_,_,_] = pms
      gDim = [roundTo 16 width, roundTo 16 height,1]
      lDim = [16,16,1]
