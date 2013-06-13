module ViperVM.Scheduling.RoundRobin (
      roundRobinScheduler
   ) where

import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler
import ViperVM.Platform.Platform (processors)
import ViperVM.Scheduling.Single

import Control.Concurrent.STM
import Control.Concurrent

import Control.Monad (void)
import Data.Traversable (forM)

import Data.Vector as Vector (Vector,length,fromList, (!)) 

-- | Scheduler with a greedy strategy
roundRobinScheduler :: Scheduler
roundRobinScheduler = Scheduler initRoundRobinScheduler


initRoundRobinScheduler :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initRoundRobinScheduler som km ch = void $ forkIO (roundRobinThread som km ch)


roundRobinThread :: SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
roundRobinThread som km ch = do

   -- Create one single scheduler per proc
   scheds <- forM procs $ \proc -> do
      pch <- newBroadcastTChanIO
      initSingleScheduler proc som km =<< atomically (dupTChan pch)
      return pch

   schedule scheds 0

   where
      procs = Vector.fromList $ processors (getSharedObjectManagerPlatform som)

      schedule :: Vector.Vector (TChan SchedMsg) -> Int -> IO ()
      schedule scheds n 
         | n >= Vector.length procs = schedule scheds 0
         | otherwise = do

               str <- atomically $ do
                  msg <- readTChan ch

                  case msg of 
                     SchedExec k _ _ -> do
                        writeTChan (scheds ! n) msg
                        return ("[RoundRobin] Forward " ++ show k ++ " to " ++ show (procs ! n))

               putStrLn str

               schedule scheds (n+1)
