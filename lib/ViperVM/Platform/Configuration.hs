module ViperVM.Platform.Configuration where

import ViperVM.Common.Logger

-- | Platform configuration
data Configuration = Configuration {
  libraryOpenCL :: String,
  logger :: LogMsg -> IO ()
}
