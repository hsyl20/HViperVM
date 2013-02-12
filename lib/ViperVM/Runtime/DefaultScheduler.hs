module ViperVM.Runtime.DefaultScheduler (
   createRuntime,
   module X
) where

import ViperVM.Runtime as X
import qualified ViperVM.Platform as Pf

createRuntime :: Pf.Platform -> IO Runtime
createRuntime pf = createDefaultRuntime pf


