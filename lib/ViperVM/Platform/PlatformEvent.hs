module ViperVM.Platform.PlatformEvent where

import ViperVM.Platform.Proc
import ViperVM.Platform.Kernel
import ViperVM.Platform.KernelParameter

data PlatformEvent = 
     Error String
   | Debug String
   | KernelStart Proc Kernel [KernelParameter]
   deriving (Show)
