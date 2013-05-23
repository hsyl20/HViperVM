module ViperVM.Reducer.Graph where

import Control.Concurrent.STM
import Control.Monad (forM)
import Control.Concurrent.Future
import Control.Applicative
import Control.Concurrent
import System.Random
import Text.Printf
import Data.Map as Map


type NodeId = Int
type DataId = Int

data Status = Inactive | Computing | Computed Expr

data Node = Node Expr (TVar Status)
            
instance Eq Node where
   (==) a b = getNodeExpr a == getNodeExpr b

instance Ord Node where
   compare a b = compare (getNodeExpr a) (getNodeExpr b)

data Expr = Symbol String 
          | App Node [Node]
          | Data DataId
          deriving (Eq,Ord)


newNode :: Expr -> STM Node
newNode e = Node e <$> (newTVar Inactive)

newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

getNodeVar :: Node -> TVar Status
getNodeVar (Node _ stat) = stat

getNodeStatus :: Node -> STM Status
getNodeStatus (Node _ stat) = readTVar stat

setNodeStatus :: Node -> Status -> STM ()
setNodeStatus (Node _ stat) s = writeTVar stat s

getNodeExpr :: Node -> Expr
getNodeExpr (Node e _) = e

reduceNode :: Node -> IO Expr
reduceNode node = do

        stat <- atomically $ do
                stat <- getNodeStatus node
                case stat of
                        Computing -> retry  -- Block if already computing
                        Inactive -> setNodeStatus node Computing >> return Computing
                        Computed _ -> return stat
        
        case stat of
                Inactive -> error "Should not be inactive"
                Computed e -> return e
                Computing -> do
                        e <- reduceExpr (getNodeExpr node)
                        atomically $ setNodeStatus node (Computed e)
                        return e


reduceExpr :: Expr -> IO Expr
reduceExpr e@(Symbol {}) = return e
reduceExpr e@(Data {}) = return e
reduceExpr (App op args) = do
        op' <- forkPromise (reduceNode op)
        args' <- forM args (forkPromise . reduceNode)

        let threads = op':args'
        redex <- forM threads get

        case redex of
                [Symbol s, Data x, Data y]  -> do
                        putStrLn (printf "Submit task %s with args %d %d then wait" s x y)
                        threadDelay =<< ((`mod` 3000000) <$> randomIO)
                        return (Data (10+x+y))
                _ -> do
                        error "Don't know what to do with this"


cse :: Node -> Node
cse = snd . cse' Map.empty
   where
      cse' :: Map Node Node -> Node -> (Map Node Node, Node)
      cse' nodes node = case Map.lookup n ns of
                            Just a -> (ns,a)
                            Nothing -> (insert n n ns, n)
         where
            (ns,n) = case getNodeExpr node of
                   Symbol {} -> (nodes,node)
                   Data {}   -> (nodes,node)
                   App op args -> (ns2, Node (App op' args') (getNodeVar node))
                        where
                           (ns1,op') = cse' nodes op
                           (ns2,args') = f ns1 args []
                           f ns3 [] as = (ns3,as)
                           f ns3 (x:xs) as = let (ns4,a) = cse' ns3 x in f ns4 xs (a:as)

