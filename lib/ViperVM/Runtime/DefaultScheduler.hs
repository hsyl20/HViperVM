module ViperVM.Runtime.DefaultScheduler (
   createRuntime,
   module X
) where

import ViperVM.Runtime as X
import qualified ViperVM.Platform as Pf

createRuntime :: Pf.Platform -> IO Runtime
createRuntime pf = do
   compiler <- startCompilerThread

   def <- createDefaultRuntime pf

   let r = def {
         notifyKernelRegister = \_ k -> compiler (k, processors def)
      }
   return r


