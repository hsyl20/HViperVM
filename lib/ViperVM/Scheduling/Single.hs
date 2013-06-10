{-# LANGUAGE LambdaCase #-}
module ViperVM.Scheduling.Single (
      singleScheduler
   ) where

import ViperVM.Platform
import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.SharedObject
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Scheduler

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void,forever,forM)
import Control.Applicative ((<$>))
import Data.Set as Set

-- | Scheduler using a single processor
singleScheduler :: Processor -> Scheduler
singleScheduler proc = Scheduler (initSingleScheduler proc)


initSingleScheduler :: Processor -> SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
initSingleScheduler proc som km ch = void $ forkIO (singleThread proc som km ch)


singleThread :: Processor -> SharedObjectManager -> KernelManager -> TChan SchedMsg -> IO ()
singleThread proc som km ch = do
   let 
      om = objectManager som
      mem = head (processorMemories proc)

   forever $ do
      msg <- atomically (readTChan ch)

      case msg of 
         SchedExec k args res -> do
            putStrLn ("[Single] Execute " ++ show k ++ " with params " ++ show args)

            -- Move input data in memory
            let argModes = args `zip` lockModes k
            args' <- forM argModes $ \(arg,mode) -> case mode of
               ReadOnly -> do
                  Set.elems <$> atomically (memoryObjects mem arg) >>= \case
                     x:_ -> return x
                     [] -> Set.elems <$> atomically (objects arg) >>= \case
                        [] -> error "Uninitialized object accessed in read-only mode"
                        src:_ -> allocateTransferAttach som arg src mem
                        -- FIXME: select source policy is not clever

               ReadWrite -> allocateInstance som arg mem

            -- Execute kernel
            executeObjectKernel om proc k args'

            -- Associate output parameters
            forM (argModes `zip` args') $ \((arg,mode),arg') -> case mode of
               ReadOnly -> return ()
               ReadWrite -> atomically (attachObject arg arg')

            -- Return result
            atomically (writeTVar res SchedSuccess)
