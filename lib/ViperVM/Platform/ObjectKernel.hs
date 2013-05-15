module ViperVM.Platform.ObjectKernel (
      KernelObjectConfig(..), ObjectKernel(..),
      executeObjectKernel,
      registerObjectKernel,
      compileObjectKernel
   ) where

import ViperVM.Platform.Kernel
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Processor
import ViperVM.Platform.Object
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region

data KernelObjectConfig = KernelObjectConfig [KernelParameter] [(Buffer,Region)] [(Buffer,Region)]

data ObjectKernel = ObjectKernel Kernel ([Object] -> KernelObjectConfig)

peerKernel :: ObjectKernel -> Kernel
peerKernel (ObjectKernel k _) = k

executeObjectKernel :: KernelManager -> Processor -> ObjectKernel -> [Object] -> IO ()
executeObjectKernel km proc ok objs = executeKernel km proc k roRegions rwRegions pms
   where
      ObjectKernel k f = ok
      KernelObjectConfig pms roRegions rwRegions = f objs

registerObjectKernel :: KernelManager -> ObjectKernel -> IO ()
registerObjectKernel km ok = registerKernel km (peerKernel ok)

compileObjectKernel :: KernelManager -> ObjectKernel -> [Processor] -> IO [Processor]
compileObjectKernel km ok = compileKernel km (peerKernel ok)
