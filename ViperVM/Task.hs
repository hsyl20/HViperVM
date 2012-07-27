module ViperVM.Task where

import ViperVM.Kernel
import ViperVM.Data

data Task = Task Kernel [Data]
