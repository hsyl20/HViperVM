{-# LANGUAGE TupleSections #-}

module ViperVM.Runtime.Compiler (
   startCompilerThread
) where

import ViperVM.Runtime.Nodes
import ViperVM.Runtime.Kernel
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad

type CompilationRequest = (Kernel,[Processor])

startCompilerThread :: IO (CompilationRequest -> STM ())
startCompilerThread = do
   ch <- newBroadcastTChanIO
   void $ forkOS $ do
      mych <- atomically (dupTChan ch)
      compilerThread mych

   return (sendRequest ch)


sendRequest :: TChan CompilationRequest -> CompilationRequest -> STM ()
sendRequest ch req@(k,ps) = do
   beforeKernelCompilation k ps
   writeTChan ch req

compilerThread :: TChan CompilationRequest -> IO ()
compilerThread = undefined
