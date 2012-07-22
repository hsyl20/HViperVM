module ViperVM.Data.Vector where

import ViperVM.Runtime
import ViperVM.Data

import Foreign.Ptr
import Control.Concurrent

-- | Map a Vector of host memory into runtime managed memory
-- You mustn't use mapped host memory
mapVector :: Runtime -> VectorDesc -> Ptr () -> IO Data
mapVector (Runtime ch) desc ptr = do
  v <- newEmptyMVar
  writeChan ch $ MapVector desc ptr v
  takeMVar v

