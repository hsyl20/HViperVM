module ViperVM.Event where

import Control.Concurrent

type Event = MVar

newEvent :: IO (Event a)
newEvent = newEmptyMVar

waitEvent :: Event a -> IO a
waitEvent = readMVar

setEvent :: Event a -> a -> IO ()
setEvent = putMVar

withNewEvent :: (Event a -> IO ()) -> IO (Event a)
withNewEvent f = do
  e <- newEvent
  f e
  return e
