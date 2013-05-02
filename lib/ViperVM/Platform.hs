-- | This module gives applications a complete view of the underlying
-- architecture (memory network and processors)
module ViperVM.Platform (
   module X
) where

import ViperVM.Backends.OpenCL as X
import ViperVM.Platform.Buffer as X
import ViperVM.Platform.DataDesc as X
import ViperVM.Platform.Kernel as X
import ViperVM.Platform.Link as X
import ViperVM.Platform.Memory as X
import ViperVM.Platform.Platform as X
import ViperVM.Platform.Processor as X
import ViperVM.Platform.Region as X
import ViperVM.Platform.Transfer as X

