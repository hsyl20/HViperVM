module ViperVM.Backends.Host.Driver (
   initHost
) where

import ViperVM.Platform.Configuration
import ViperVM.Platform.LinkCapabilities
import qualified Data.Set as Set
import ViperVM.Backends.Host.Link

initHost :: Configuration -> IO [Link]
initHost _ = do
   let caps = Set.fromList [Transfer2D]
       link = Link caps

   return [link]
