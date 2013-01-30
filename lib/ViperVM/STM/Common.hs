module ViperVM.STM.Common where

import Control.Concurrent.STM
import Control.Monad

(>=$>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(>=$>) a f = a >=> return . f

withTVar :: (a -> a) -> TVar a -> STM ()
withTVar f v = readTVar v >>= writeTVar v . f

