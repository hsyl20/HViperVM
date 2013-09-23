module ViperVM.Runtime.Logger where

import System.Clock
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Control.Monad (forever)

data LogMsg = CustomLog String
            | DebugLog String
            | ErrorLog String
            | Clocked LogMsg TimeSpec
            | NullMsg
              deriving (Show,Eq)
      

data LogLevel = LogInfo | LogDebug
                deriving (Eq)


-- | Null logger
nullLogger :: LogMsg -> IO ()
nullLogger _ = return ()

filterLevel :: LogLevel -> LogMsg -> LogMsg
filterLevel level msg@(DebugLog _) 
   | level == LogDebug = msg
   | otherwise = NullMsg
filterLevel _ msg = msg


-- | Print log messages on standard output
stdOutLogger :: LogMsg -> IO ()
stdOutLogger NullMsg = return ()
stdOutLogger (CustomLog msg) = putStrLn msg
stdOutLogger (ErrorLog msg) = putStrLn ("Error: " ++ msg)
stdOutLogger (DebugLog msg) = putStrLn ("Debug: " ++ msg)
stdOutLogger (Clocked msg t) = putStr (show t) >> putStr ": " >> stdOutLogger msg

-- | Add a clock to the logger
clocked :: LogMsg -> IO LogMsg
clocked NullMsg = return NullMsg
clocked msg = Clocked msg <$> getTime Monotonic

-- | Thread safe logging
threadSafe :: (LogMsg -> IO ()) -> IO (LogMsg -> IO ())
threadSafe f = do
   ch <- newBroadcastTChanIO

   _ <- forkIO $ do
      ch' <- atomically (dupTChan ch)
      forever (atomically (readTChan ch') >>= f)

   return (atomically . writeTChan ch)

