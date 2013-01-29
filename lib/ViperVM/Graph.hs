-- | STM Graph
module ViperVM.Graph (
   Graph, Node,
   newGraph, addNode, addNode_, removeNode, printGraph, nodeValue, setNodeValue,
   tailEndpoints, headEndpoints, leaves, roots,
) where

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Data.IntMap
import Data.Traversable (traverse)
import Data.IntSet
import Text.Printf
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

-- | The task graph structure
data Graph a = Graph {
   lastId :: TVar Int,
   nodes :: TVar (IntMap (TVar a)),
   edges :: TVar (IntMap (TVar NodeSet))
}

type Node = Int
type NodeSet = IntSet


-- | Create a new graph
newGraph :: STM (Graph a)
newGraph = Graph <$> newTVar 0 <*> newTVar IntMap.empty <*> newTVar IntMap.empty


-- | Add a node in the graph
addNode :: Graph a -> a -> NodeSet -> STM Node
addNode g v deps = do
   oldId    <- readTVar (lastId g)
   oldNodes <- readTVar (nodes g)
   oldEdges <- readTVar (edges g)

   -- Update last ID
   let nodeId = oldId + 1

   writeTVar (lastId g) nodeId

   -- Add node
   nodValue <- newTVar v
   writeTVar (nodes g) $ IntMap.insert nodeId nodValue oldNodes

   -- Add valid edges
   validDeps <- newTVar $ keysSet oldEdges `IntSet.intersection` deps

   writeTVar (edges g) $ IntMap.insert nodeId validDeps oldEdges

   return nodeId


-- | Add a node without returning it
addNode_ :: Graph a -> a -> NodeSet -> STM ()
addNode_ g v ds = void $ addNode g v ds


-- | Filter a set of nodes
filterNode :: Node -> TVar NodeSet -> STM ()
filterNode n s = do
   old <- readTVar s
   let new = IntSet.filter (/=n) old
   if old /= new
      then writeTVar s new
      else return ()


-- | Remove a node from the graph
removeNode :: Graph a -> Node -> STM ()
removeNode g n = do
   oldNodes <- readTVar (nodes g)
   oldEdges <- readTVar (edges g)

   -- Remove node
   writeTVar (nodes g) $ IntMap.delete n oldNodes

   -- Remove node edges
   let newEdges = IntMap.delete n oldEdges
   writeTVar (edges g) newEdges

   -- Remove edges targeting the node
   forM_ (IntMap.elems newEdges) (filterNode n)
   

-- | Return the value associated to a node
nodeValue :: Graph a -> Node -> STM a
nodeValue g n = do
   oldNodes <- readTVar (nodes g)
   value <- readTVar $ oldNodes ! n
   return $ value


-- | Set the value of a node
setNodeValue :: Graph a -> Node -> a -> STM ()
setNodeValue g n v = do
   oldNodes <- readTVar (nodes g)
   writeTVar (oldNodes ! n) v
   

-- | Print a graph representation
printGraph :: Show a => Graph a -> STM String
printGraph g = do
   oldNodes <- readTVar (nodes g)
   oldEdges <- readTVar (edges g)
   
   strs <- forM (IntMap.assocs oldNodes) $ \(k,v) -> do
      value <- readTVar v
      deps <- IntSet.toList <$> (readTVar $ oldEdges ! k)
      return $ printf "%d --> %s\t%s\n" k (show deps) (show value)

   return $ concat strs

-- | Retrieve filtered edges
filterEdges :: Graph a -> (NodeSet -> Bool) -> STM NodeSet
filterEdges g f = do
   oldEdges <- readTVar (edges g)
   IntMap.keysSet . IntMap.filter f <$> traverse readTVar oldEdges

-- | Return outgoing edges endpoints
tailEndpoints :: Graph a -> Node -> STM NodeSet
tailEndpoints g n = do
   oldEdges <- readTVar (edges g)
   readTVar (oldEdges ! n)


-- | Return incoming edges endpoints
headEndpoints :: Graph a -> Node -> STM NodeSet
headEndpoints g n = filterEdges g (IntSet.member n)
      
-- | Retrieve leaf nodes
leaves :: Graph a -> STM NodeSet
leaves g = filterEdges g IntSet.null
   
-- | Retrieve leaf nodes
roots :: Graph a -> STM NodeSet
roots g = do
   oldEdges <- readTVar (edges g)
   IntMap.foldl IntSet.difference (IntMap.keysSet oldEdges) <$> traverse readTVar oldEdges
   
