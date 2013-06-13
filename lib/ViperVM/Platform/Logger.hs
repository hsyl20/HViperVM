module ViperVM.Platform.Logger where

data LogMsg = CustomLog String
            | DebugLog String
            | ErrorLog String
              deriving (Show,Eq)
      

data LogLevel = LogInfo | LogDebug
                deriving (Eq)

-- | Null logger
nullLogger :: LogMsg -> IO ()
nullLogger _ = return ()

-- | Print log messages on standard output
stdOutLogger :: LogLevel -> LogMsg -> IO ()
stdOutLogger _ (ErrorLog msg) = putStrLn ("Error: " ++ msg)
stdOutLogger _ (CustomLog msg) = putStrLn msg
stdOutLogger level (DebugLog msg) 
   | level == LogDebug = putStrLn msg
   | otherwise = return ()
