module ViperVM.Platform.Scheduler where

import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.SharedObject
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager

import Control.Concurrent.STM

data SchedMsg = SchedExec ObjectKernel [SharedObject] TSchedResult

data SchedResult = SchedNoResult | SchedSuccess | SchedError
                   deriving (Eq)

type TSchedResult = TVar SchedResult

data Scheduler = Scheduler {
      initScheduler :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
   }

