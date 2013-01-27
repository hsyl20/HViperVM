module ViperVM.RequestManager (
   RequestManager,
   ViperVM.RequestManager.init,
   submit,
) where

import Data.IntMap
import qualified Data.IntMap as IntMap

type Request = Int
type RequestId = Int

data RequestNode = RequestNode RequestId Request [RequestId]

-- | Manage the graph of requests
data RequestManager = RequestManager {
   lastId :: RequestId,                      -- ^ Last free id
   requests :: IntMap RequestNode,           -- ^ All requests
   roots :: [RequestId],                     -- ^ Roots of the request graph
   leafs :: [RequestId],                     -- ^ Leafs of the request graph
   active :: [RequestId]                     -- ^ Requests being fulfilled
}

-- | Initialize a request manager
init :: RequestManager
init = RequestManager 0 IntMap.empty [] [] []

-- | Submit a new request
submit :: RequestManager -> Request -> RequestManager
submit manager r = RequestManager nextId reqs (roots manager) (leafs manager) (active manager)
   where
      nextId = lastId manager
      node = RequestNode nextId r []
      reqs = IntMap.insert nextId node (requests manager)

