{-# LANGUAGE TupleSections #-} 

module ViperVM.Internals.StartStop (
  startRuntime
  ) where

import ViperVM.Internals.Structures
import ViperVM.Logging.Logger
import ViperVM.Event
import ViperVM.Platform (processors, links, Platform)
--import ViperVM.Internals.Logging

import Control.Monad.State (evalStateT,gets)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad (void, replicateM)
import Control.Applicative (liftA2, (<$>))
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified ViperVM.Internals.BufferManager as BufferManager
import qualified ViperVM.Internals.RegionManager as RegionManager
import qualified ViperVM.Internals.InstanceManager as InstanceManager
import qualified ViperVM.Internals.DataManager as DataManager

-- | Starts the runtime on the given platform
startRuntime :: Platform -> Logger -> Scheduler -> IO Runtime
startRuntime pf l s = do
  ch <- newChan
  -- Create one chan per link
  lkChans <- Map.fromList <$> liftA2 zip (return $ links pf) (replicateM (length $ links pf) newChan)
  let st = RuntimeState {
    channel = ch,
    platform = pf,
    logger = l,
    scheduler = s,
    linkChannels = lkChans,
    _bufferManager = BufferManager.init,
    _regionManager = RegionManager.init,
    _instanceManager = InstanceManager.init,
    _dataManager = DataManager.init,
    _dataEvents = Map.empty,
    _dataTasks = Map.empty,
    _invalidDataInstances = Map.empty,
    _compiledKernels = Map.empty,
    _submittedTasks = [],
    _scheduledTasks = Map.fromList $ fmap (,[]) (processors pf),
    _requestTasks = Map.empty,
    _taskRequests = Map.empty,
    _activeRequests = Set.empty
  }
  void $ forkIO $ do
    logInfo l "Runtime started"
    msg <- evalStateT runtimeLoop st
    logInfo l "Stopping the runtime..."
    logInfo l "This log will now be closed"
    shutdownLogger l
    -- Signal that runtime is stopped to the application
    let (AppQuit ev) = msg
    setEvent ev ()
  return $ Runtime ch

-- | Main runtime loop
runtimeLoop :: R Message
runtimeLoop = do
  msg <- withStateR (readChan . channel)

  sched <- gets scheduler
  sched msg

  if isQuit msg then return msg else runtimeLoop

-- | Shutdown the logger
shutdownLogger :: Logger -> IO ()
shutdownLogger l = void $ withNewEvent $ \e -> do
  l $ StopLogger e
  waitEvent e
  
