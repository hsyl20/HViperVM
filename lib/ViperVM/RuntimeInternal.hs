module ViperVM.RuntimeInternal (
  -- Methods
  registerBufferR, registerDataInstanceR, newData,
  setEventR, timeActionR,
  postMessageR, kpToTp
  ) where

import Prelude hiding (lookup)

import Control.Monad.State (lift)

import ViperVM.Internals.Structures
import ViperVM.Internals.Memory

import ViperVM.Data
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
kpToTp (KPReadWrite d) = do
  d2 <- newData (dataDescriptor d)
  return $ TPReadWrite d d2
kpToTp (KPAllocate dd) = do
  d2 <- newData dd
  return $ TPAllocate d2