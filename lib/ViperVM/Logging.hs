module ViperVM.Logging (
  logCustomR, logErrorR, logWarningR, logInfoR 
  ) where

import ViperVM.Structures
import ViperVM.Logging.Logger

-- | Write a custom string message in the log
logCustomR :: String -> String -> R ()
logCustomR header s = withStateR $ (\l -> logCustom l header s) . logger

-- | Write a custom error message in the log
logErrorR :: String -> R ()
logErrorR s = withStateR $ flip logError s . logger

-- | Write a custom warning message in the log
logWarningR :: String -> R ()
logWarningR s = withStateR $ flip logWarning s . logger

-- | Write a custom info message in the log
logInfoR :: String -> R ()
logInfoR s = withStateR $ flip logInfo s . logger
