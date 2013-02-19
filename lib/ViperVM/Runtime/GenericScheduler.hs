module ViperVM.Runtime.GenericScheduler where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import ViperVM.Runtime.Nodes

data SchedEvent = Submitted Task | Completed TaskInstance

createGenericScheduler :: (SchedEvent -> STM ()) -> IO (SchedEvent -> STM ())
createGenericScheduler sched = do
   ch <- newBroadcastTChanIO
   void $ forkOS $ do
      mych <- atomically (dupTChan ch)
      schedulerThread mych sched

   return (writeTChan ch)


schedulerThread :: TChan SchedEvent -> (SchedEvent -> STM ()) -> IO ()
schedulerThread ch sched = forever $ atomically $ (readTChan ch >>= sched)
