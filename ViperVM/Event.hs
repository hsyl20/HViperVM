module ViperVM.Event where

import Control.Concurrent

type Event = MVar

newEvent :: IO (Event a)
newEvent = newEmptyMVar

waitEvent :: Event a -> IO a
waitEvent = readMVar

setEvent :: Event a -> a -> IO ()
setEvent event value = putMVar event value

withNewEvent :: (Event a -> IO ()) -> IO (Event a)
withNewEvent f = do
  e <- newEvent
  f e
  return e
