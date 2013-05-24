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

instance Show Node where
   show (Node x _) = show x

data Expr = Symbol String 
          | App Node [Node]
          | Data DataId
          | Var Int
          | Abs Node
          | ConstInteger Integer
          | ConstBool Bool
          deriving (Eq,Ord)

instance Show Expr where
   show (Symbol s) = s
   show (App op args) = "(" ++ show op ++ (concat $ fmap (\x -> " " ++ show x) args) ++ ")"
   show (Data i) = "#" ++ show i
   show (Var i) = "v" ++ show i
   show (Abs n) = "λ. " ++ show n
   show (ConstInteger i) = show i
   show (ConstBool i) = show i

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

reduceNode :: Map String Node -> Node -> IO Expr
reduceNode ctx node = do

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
                  e <- reduceExpr ctx (getNodeExpr node)
                  atomically $ setNodeStatus node (Computed e)
                  return e


reduceExpr :: Map String Node -> Expr -> IO Expr

reduceExpr ctx (Symbol s) | Map.member s ctx =
   return $ getNodeExpr (ctx Map.! s)

reduceExpr ctx ea@(App op args) = do
        op' <- forkPromise (reduceNode ctx op)
        args' <- forM args (forkPromise . reduceNode ctx)

        let threads = op':args'
        redex <- forM threads get

        case redex of
                [Abs e, e'] -> reduceNode ctx =<< shiftReplaceNode 0 e' e

                Abs e:e':_ -> do
                  op'' <- shiftReplaceNode 0 e' e
                  reduceExpr ctx $ App op'' (tail args)

                App op2 args2:_ -> reduceExpr ctx $ App op2 (args2 ++ args)

                [Symbol "+", ConstInteger x, ConstInteger y] -> 
                        return (ConstInteger (x+y))

                [Symbol "-", ConstInteger x, ConstInteger y] -> 
                        return (ConstInteger (x-y))

                [Symbol "*", ConstInteger x, ConstInteger y] -> 
                        return (ConstInteger (x*y))

                [Symbol s, Data x, Data y]  -> do
                        putStrLn (printf "Submit task %s with args %d %d then wait" s x y)
                        threadDelay =<< ((`mod` 1000000) <$> randomIO)
                        return (Data (x+y))

                _ -> return ea

reduceExpr _ e = return e

shiftReplaceNode :: Int -> Expr -> Node-> IO Node
shiftReplaceNode i e n = newNodeIO =<< shiftReplace i e (getNodeExpr n)

-- | Replace Var with given Expr and shift all other Vars
shiftReplace :: Int -> Expr -> Expr -> IO Expr
shiftReplace i n (Var j) | i == j = return n
                         | j > i = return (Var (j-1))
                         | j < i = return (Var j)
shiftReplace i n (Abs e) = Abs <$> shiftReplaceNode (i+1) n e
shiftReplace i n (App op args) = do
   op' <- shiftReplaceNode i n op
   args' <- forM args (shiftReplaceNode i n)
   return (App op' args')
shiftReplace _ _ e = return e


cse :: Node -> Node
cse = snd . cse' Map.empty
   where
      cse' :: Map Node Node -> Node -> (Map Node Node, Node)
      cse' nodes node = case Map.lookup n ns of
                            Just a -> (ns,a)
                            Nothing -> (insert n n ns, n)
         where
            (ns,n) = case getNodeExpr node of
                   Abs e -> let (_,n') = cse' nodes e in (nodes, Node (Abs n') (getNodeVar node)) -- we do not return ns as it could contain Var
                   App op args -> (ns2, Node (App op' args') (getNodeVar node))
                        where
                           (ns1,op') = cse' nodes op
                           (ns2,args') = f ns1 args []
                           f ns3 [] as = (ns3,as)
                           f ns3 (x:xs) as = let (ns4,a) = cse' ns3 x in f ns4 xs (a:as)
                   _ -> (nodes,node)

