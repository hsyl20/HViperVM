module ViperVM.Logging.TextLogger (
  newTextLogger
  ) where

import ViperVM.Logging.Logger
import ViperVM.Platform

import System.IO
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad ( void )
import Data.Functor ( (<$>) )
import Data.Traversable
import Data.Maybe ( isJust )

data TextLogger = TextLogger Handle (Chan LogMessage)

-- | Create a new logger that output messages on the specified handle
newTextLogger :: Handle -> IO Logger
newTextLogger hdl = do
  ch <- newChan
  void $ forkIO $ textLoggerLoop $ TextLogger hdl ch
  let logger msg = writeChan ch msg
  return logger

textLoggerLoop :: TextLogger -> IO ()
textLoggerLoop l@(TextLogger hdl ch) = do
  msg <- readChan ch
  case msg of
    StopLogger -> return ()
    m -> handleMsg m >> textLoggerLoop l

  where
    mPutStrLn = hPutStrLn hdl

    handleMsg StopLogger = return ()

    handleMsg (Custom s) = mPutStrLn s

    handleMsg (KernelCompilation k procs ck act) = do
      mPutStrLn $ "Compilation of kernel " ++ (show k) ++ ":"
      let f c | isJust c  = "  - Compiled successfully for "
              | otherwise = "  - Compilation failed for "
      procInfos <- traverse procInfo procs
      void $ traverse mPutStrLn $ zipWith (++) (f <$> ck) procInfos


