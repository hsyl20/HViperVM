module ViperVM.Backends.Host.Driver (
   initHost
) where

import ViperVM.Platform.Configuration

import ViperVM.Backends.Host.Link
import ViperVM.Backends.Host.Memory

-- | Initialize host driver
initHost :: Configuration -> IO ([Memory], [Link])
initHost _ = do

   m <- initMemory
   let l = initLink m

   return ([m],[l])
