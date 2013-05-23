module ViperVM.Reducer.Graph where

import Control.Concurrent.STM
import Control.Monad (forM)
import Control.Concurrent.Future
import Control.Applicative
import Text.Printf

type NodeId = Int
type DataId = Int

data Status = Inactive | Computing | Computed Expr

data Node = Node Expr (TVar Status)

data Expr = Symbol String 
          | App Node [Node]
          | Data DataId


newNode :: Expr -> STM Node
newNode e = Node e <$> (newTVar Inactive)

newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

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
                        return (Data (10+x+y))
                _ -> do
                        error "Don't know what to do with this"
