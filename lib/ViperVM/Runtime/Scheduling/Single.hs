{-# LANGUAGE LambdaCase #-}
module ViperVM.Runtime.Scheduling.Single (
   singleScheduler
) where

import ViperVM.Platform.Proc
import ViperVM.Platform.Kernel
import ViperVM.Platform.Platform
import ViperVM.VirtualPlatform.Object
import ViperVM.VirtualPlatform.Task
import ViperVM.VirtualPlatform.MetaKernel (kernels, AccessMode(..))
import ViperVM.Runtime.Scheduler

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever)
import Text.Printf

-- | Scheduler using a single processor
singleScheduler :: Platform -> Proc -> IO Scheduler
singleScheduler pf proc = do
   chan <- newBroadcastTChanIO

   -- Start scheduler thread
   chan' <- atomically (dupTChan chan)
   _ <- forkIO $ singleThread pf proc chan'

   return $ Scheduler pf (submitTask chan)

submitTask :: TChan (Task,TaskEvent) -> (Task -> IO TaskEvent)
submitTask chan task = do
   ev <- initEvent
   atomically (writeTChan chan (task,ev))
   return ev


singleThread :: Platform -> Proc -> TChan (Task,TaskEvent) -> IO ()
singleThread pf proc ch = forever $ do

   (task,ev) <- atomically (readTChan ch)
            
   let 
      metaKer = metaKernel task
      metaObjs = params task
      mems = procMemories proc

   -- Select and compile kernel
   kernel <- prepareKernel pf proc task

   -- Prepare objects
   objs <- prepareObjects pf proc task kernel

   -- Execute kernel
   --customLog pf (printf "[Single %s] Executing %s with params %s " (show proc) (show kernel) (show objs))
   taskExecute task kernel proc objs
   --customLog pf (printf "[Single %s] Execution complete" (show proc))

   -- Unprepare objects
   unprepareObjects pf proc task kernel objs

   -- Notify task completion
   setEvent ev


-- | Select and compile a kernel for the given processor
prepareKernel :: Platform -> Proc -> Task -> IO Kernel
prepareKernel pf proc task = do
   let 
      allKernels = kernels (metaKernel task)
      validKernels = filter (canExecute proc) allKernels

   --TODO: compile kernel
   let k = head validKernels

   return k


-- | Prepare parameters
-- Allocate and lock output objects
-- Select, transfer/duplicate and lock input objects
prepareObjects :: Platform -> Proc -> Task -> Kernel -> IO [Object]
prepareObjects pf proc task kernel = do
   let
      roObjs = taskParamsWithMode task ReadOnly
      rwObjs = taskParamsWithMode task ReadWrite
      woObjs = taskParamsWithMode task WriteOnly

   --TODO
   return []


-- | Unprepare parameters
-- Associate output objects with meta objects
-- Unlock objects
unprepareObjects :: Platform -> Proc -> Task -> Kernel -> [Object] -> IO ()
unprepareObjects pf proc task kernel objs = do
   --TODO
   return ()
