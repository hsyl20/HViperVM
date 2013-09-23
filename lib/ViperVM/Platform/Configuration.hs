module ViperVM.Platform.Configuration where

import ViperVM.Platform.PlatformEvent

-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String,
  eventHandler :: PlatformEvent -> IO ()
}
