{-# LANGUAGE LambdaCase #-}
-- | Command Graph model
module ViperVM.Interfaces.CommandGraph where

import ViperVM.Backends.Common.Buffer
import ViperVM.Platform.Memory
import ViperVM.Platform.Kernel
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.Proc
import ViperVM.Platform.Link
import ViperVM.Common.Region

import Control.Applicative ( (<$>) )
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (void)
import Data.Word
import Data.Foldable

data Status = Inactive | Active | Completed | Failed

data Command = Command (TVar Status)

-- | Create a command
newCommand :: IO Command
newCommand = Command <$> newTVarIO Inactive

setStatus :: Status -> Command -> STM ()
setStatus s (Command status) = writeTVar status s

-- | Wait until a command completes
waitForCompletion :: Command -> STM ()
waitForCompletion (Command status) = do
   readTVar status >>= \case
      Completed -> return ()
      _ -> retry

-- | Wait until several commands complete
waitForCompletions :: [Command] -> STM ()
waitForCompletions = traverse_ waitForCompletion

-- | Execute a callback after the completion of several commands
after :: [Command] -> IO () -> IO ()
after cmds cb = void $ forkIO $ atomically (waitForCompletions cmds) >> cb

-- | Allocate a buffer
allocateBuffer :: Word64 -> Memory -> [Command] -> IO (Command, TMVar (AllocResult Buffer))
allocateBuffer sz mem deps = do
   res <- newEmptyTMVarIO
   cmd <- newCommand

   after deps $ do
      atomically $ setStatus Active cmd
      buf <- bufferAllocate sz mem
      atomically $ do
         putTMVar res buf
         setStatus Completed cmd
   return (cmd,res)

-- | Release a buffer
releaseBuffer :: Buffer -> [Command] -> IO Command
releaseBuffer b deps = do
   cmd <- newCommand
   after deps $ do
      atomically $ setStatus Active cmd
      bufferRelease b
      atomically $ setStatus Completed cmd
   return cmd
