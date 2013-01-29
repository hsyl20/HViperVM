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
data GraphS a = GraphS Int (IntMap (NodeS a))  -- ^ GraphS lastId nodeEdges
data NodeS a = NodeS (TVar a) (TVar NodeSet)   -- ^ NodeS value edges

type Node = Int
type NodeSet = IntSet

newtype Graph a = Graph (TVar (GraphS a))

-- | Create a new graph
newGraph :: STM (Graph a)
newGraph = Graph <$> newTVar (GraphS 0 IntMap.empty)

-- | Retrieve graph last used Id
lastId :: Graph a -> STM Int
lastId (Graph g) = do
   (GraphS l _) <- readTVar g
   return l

-- | Retrieve values and edges of a graph
valueEdges :: Graph a -> STM (IntMap (NodeS a))
valueEdges (Graph g) = do
   (GraphS _ ns) <- readTVar g
   return ns

-- | Retrieve graph edges
edges :: Graph a -> STM (IntMap (TVar NodeSet))
edges g = fmap (\(NodeS _ s) -> s) <$> valueEdges g

-- | Retrieve graph nodes
nodes :: Graph a -> STM NodeSet
nodes g = IntMap.keysSet <$> edges g

-- | Retrieve graph values
values :: Graph a -> STM (IntMap (TVar a))
values g = fmap (\(NodeS v _) -> v) <$> valueEdges g

-- | Retrieve node value TVar
nodeValueT :: Graph a -> Node -> STM (TVar a)
nodeValueT g n = flip (!) n <$> values g

-- | Retrieve value of a node
nodeValue :: Graph a -> Node -> STM a
nodeValue g n = readTVar =<< nodeValueT g n

-- | Retrieve filtered edges
filterEdges :: Graph a -> (NodeSet -> Bool) -> STM NodeSet
filterEdges g f = IntMap.keysSet . IntMap.filter f <$> (traverse readTVar =<< edges g)

-- | Return outgoing edges endpoints
tailEndpoints :: Graph a -> Node -> STM NodeSet
tailEndpoints g n = readTVar =<< flip (!) n <$> edges g

-- | Return incoming edges endpoints
headEndpoints :: Graph a -> Node -> STM NodeSet
headEndpoints g n = filterEdges g (IntSet.member n)
      
-- | Retrieve leaf nodes
leaves :: Graph a -> STM NodeSet
leaves g = filterEdges g IntSet.null
   
-- | Retrieve root nodes
roots :: Graph a -> STM NodeSet
roots g = IntMap.foldl IntSet.difference <$> (nodes g) <*> (traverse readTVar =<< edges g)
   
-- | Find the next free ID return it
fetchFreeId :: Graph g -> STM Int
fetchFreeId g = do
   oldId <- lastId g
   ks <- nodes g

   let ids = iterate (+1) (oldId+1)
   return $ head $ dropWhile (flip IntSet.member ks) ids

-- | Add a node in the graph
addNode :: Graph a -> a -> NodeSet -> STM Node
addNode g@(Graph g1) v deps = do

   -- Create node with only valid edges
   nodeId <- fetchFreeId g
   validDeps <- IntSet.intersection deps <$> nodes g
   node <- NodeS <$> newTVar v <*> newTVar validDeps

   -- Update the graph
   g2 <- GraphS nodeId . IntMap.insert nodeId node <$> valueEdges g
   writeTVar g1 g2

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
removeNode g@(Graph g1) n = do
   (GraphS lsId vs) <- readTVar g1

   -- Remove node
   writeTVar g1 $ GraphS lsId (IntMap.delete n vs)

   -- Remove edges targeting the node
   es <- edges g
   forM_ (IntMap.elems es) (filterNode n)


   
-- | Set the value of a node
setNodeValue :: Graph a -> Node -> a -> STM ()
setNodeValue g n v = flip writeTVar v =<< nodeValueT g n
   

-- | Print a graph representation
printGraph :: Show a => Graph a -> STM String
printGraph g = do
   oldNodes <- valueEdges g
   
   strs <- forM (IntMap.assocs oldNodes) $ \(k, NodeS v ds) -> do
      value <- readTVar v
      deps <- IntSet.toList <$> readTVar ds
      return $ printf "%d --> %s\t%s\n" k (show deps) (show value)

   return $ concat strs

