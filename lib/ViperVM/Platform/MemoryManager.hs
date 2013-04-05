module ViperVM.Platform.MemoryManager where

import ViperVM.Platform.Memory
import ViperVM.Platform.Buffer
import ViperVM.Platform.Platform

import ViperVM.STM.TSet as TSet

import Control.Concurrent.STM
import Control.Monad (forM)
import Control.Applicative ( (<$>) )
import Data.Map

data MemoryManager = MemoryManager {
                        buffers :: Map Memory (TSet Buffer)
                     }

createMemoryManager :: Platform -> IO MemoryManager
createMemoryManager pf = do
   let mems = memories pf
   bufs <- fromList . (mems `zip`) <$> (atomically $ forM mems (\_ -> TSet.empty))
      
   return $ MemoryManager bufs
