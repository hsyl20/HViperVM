module ViperVM.RuntimeInternal (
  -- Methods
  setEventR, timeActionR,
  postMessageR, kpToTp
  ) where

import Prelude hiding (lookup)

import Control.Monad.State (lift)
import Control.Applicative

import ViperVM.Structures
import ViperVM.Memory

import ViperVM.Event
import ViperVM.KernelInterface
import ViperVM.Logging.Logger
import ViperVM.Task

-- | Execute an action and return its start and end times
timeActionR :: IO a -> R (a,TimedAction)
timeActionR = lift . timeAction

-- Set an event in the R monad
setEventR :: Event a -> a -> R ()
setEventR ev v = lift $ setEvent ev v

-- | Convert kernel parameter into task parameter, allocating data when necessary
kpToTp :: KernelParameter -> R TaskParameter
kpToTp (KPReadOnly d) = return $ (TPReadOnly d)
kpToTp (KPReadWrite d) = TPReadWrite d <$> (allocateDataR =<< descriptorR d)
kpToTp (KPAllocate dd) = TPAllocate <$> allocateDataR dd
