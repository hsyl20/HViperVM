-- | Event Management Module. Events are containers for values that are
-- computed asynchronously
module ViperVM.Event (
  Event, OwnedEvent,
  newEvent, waitEvent, setEvent, withNewEvent,publishEvent
 ) where

import Control.Concurrent

-- | Event that can be waited for and read
type Event = MVar

-- | Event whose value can be set
type OwnedEvent = Event

-- | Create a new event
newEvent :: IO (OwnedEvent a)
newEvent = newEmptyMVar

-- | Wait for an event to complete and return its value
waitEvent :: Event a -> IO a
waitEvent = readMVar

-- | Set the value of an event
setEvent :: OwnedEvent a -> a -> IO ()
setEvent = putMVar

-- | Publish an event for other to wait and read
publishEvent :: OwnedEvent a -> Event a
publishEvent = id

-- | Execute the given function with a newly created event, then return it
withNewEvent :: (OwnedEvent a -> IO ()) -> IO (Event a)
withNewEvent f = do
  e <- newEvent
  f e
  return $ publishEvent e
