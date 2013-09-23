module ViperVM.Runtime.Scheduler where

import ViperVM.Platform.Platform
import ViperVM.VirtualPlatform.Task

import Control.Applicative ((<$>))
import Control.Concurrent.STM

data Scheduler = Scheduler {
   platform :: Platform,
   submit :: Task -> IO TaskEvent
}

newtype TaskEvent = TaskEvent (TVar Bool)

initEvent :: IO TaskEvent
initEvent = TaskEvent <$> newTVarIO False

setEvent :: TaskEvent -> IO ()
setEvent (TaskEvent ev) = atomically (writeTVar ev True)
