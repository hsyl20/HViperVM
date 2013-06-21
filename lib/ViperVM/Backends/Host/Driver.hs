module ViperVM.Backends.Host.Driver (
   initHost
) where

import ViperVM.Platform.Configuration
import qualified ViperVM.Platform.Memory as PF

import ViperVM.Backends.Host.Link
import ViperVM.Backends.Host.Memory

-- | Initialize host driver
initHost :: Configuration -> IO ([Memory], [Link])
initHost _ = do

   m <- initMemory
   let l = initLink (PF.HostMemory m)

   return ([m],[l])
