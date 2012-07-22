module ViperVM.Logging.NullLogger where

import ViperVM.Logging.Logger

nullLogger :: Logger
nullLogger _ = return ()
