-- | This module gives applications a complete view of the underlying
-- architecture (memory network and processors)
module ViperVM.Platform (
   module X
) where

import ViperVM.Platform.Memory as X
import ViperVM.Platform.Link as X
import ViperVM.Platform.Proc as X
import ViperVM.Platform.Platform as X
