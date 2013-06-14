module ViperVM.Platform.Configuration where

import ViperVM.Platform.Logger

-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String,
  logger :: LogMsg -> IO ()
}
