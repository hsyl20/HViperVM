{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.MemoryManager (
   MemoryManager,
   ViperVM.MemoryManager.init
) where

import ViperVM.Platform
import ViperVM.BufferManager 
import qualified ViperVM.BufferManager as BufferManager
import ViperVM.RegionManager 
import qualified ViperVM.RegionManager as RegionManager
import ViperVM.TransferManager 
import qualified ViperVM.TransferManager as TransferManager

import Data.Lens.Lazy
import Data.Lens.Template


-- | Manage memory allocations, releases and transfers
data MemoryManager = MemoryManager {
      platform :: Platform,
      _bufferManager :: BufferManager,
      _regionManager :: RegionManager,
      _transferManager :: TransferManager
   }

$( makeLens ''MemoryManager )

-- | Initialize memory manager
init :: Platform -> IO MemoryManager
init pf = return $ MemoryManager {
      platform = pf,
      _bufferManager = BufferManager.init,
      _regionManager = RegionManager.init,
      _transferManager = TransferManager.init
   }
