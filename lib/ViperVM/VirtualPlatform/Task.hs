module ViperVM.VirtualPlatform.Task (
   Task(..)
) where

import ViperVM.VirtualPlatform.MetaKernel
import ViperVM.VirtualPlatform.MetaObject

-- | Task
data Task = Task {
   metaKernel :: MetaKernel,
   params :: [MetaObject]
}
