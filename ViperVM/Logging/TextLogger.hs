module ViperVM.Logging.TextLogger (
  newTextLogger
  ) where

import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Event

import System.IO
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad ( void, unless )
import Data.Functor ( (<$>) )
import Data.Traversable
import Data.Maybe ( isJust )
import Text.Printf

data TextLogger = TextLogger Handle (Chan LogMessage)

-- | Create a new logger that output messages on the specified handle
newTextLogger :: Handle -> IO Logger
newTextLogger hdl = do
  ch <- newChan
  void $ forkIO $ textLoggerLoop $ TextLogger hdl ch
  let logger msg = writeChan ch msg
  return logger

isStop :: LogMessage -> Bool
isStop (StopLogger {}) = True
isStop _ = False

textLoggerLoop :: TextLogger -> IO ()
textLoggerLoop l@(TextLogger hdl ch) = do
  msg <- readChan ch
  handleMsg msg
  unless (isStop msg) $ textLoggerLoop l

  where
    mPutStrLn = hPutStrLn hdl

    handleMsg (StopLogger ev) = do
      hFlush hdl
      setEvent ev ()

    handleMsg (Custom s) = mPutStrLn s

    handleMsg (KernelCompilation k procs ck act) = do
      mPutStrLn $ printf "Compilation of %s took %s:" (show k) (durationSecs act)
      let f c | isJust c  = "  - Compilation succeeded for "
              | otherwise = "  - Compilation failed for "
      procInfos <- traverse procInfo procs
      void $ traverse mPutStrLn $ zipWith (++) (f <$> ck) procInfos


