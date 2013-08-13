module ViperVM.VirtualPlatform.MetaMemory (
   MetaMemory, initMetaMemory,
   allocateMetaObject
) where

import ViperVM.Platform.Platform
import ViperVM.VirtualPlatform.MetaObject as MO
import ViperVM.VirtualPlatform.Descriptor
import ViperVM.STM.TSet as TSet

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )

-- | Memory of meta objects that keeps a pointer on each of them
data MetaMemory = MetaMemory {
   metaMemoryPlatform :: Platform,
   metaObjects :: TSet MetaObject
}

initMetaMemory :: Platform -> IO MetaMemory
initMetaMemory pf = MetaMemory pf <$> atomically TSet.empty

allocateMetaObject :: MetaMemory -> Descriptor -> IO MetaObject
allocateMetaObject mm desc = do
   mo <- MO.allocate desc
   atomically $ TSet.insert mo (metaObjects mm)
   return mo
