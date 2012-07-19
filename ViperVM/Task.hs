module ViperVM.Task where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Data

data Kernel = CLKer CLKernel

data Task = Task Kernel
