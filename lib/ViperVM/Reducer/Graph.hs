{-# LANGUAGE LambdaCase #-}
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
   opFuture <- forkPromise (reduceNode ctx op)

   -- If we want to be greedy, we can reduce "args" here too.
   -- However, we may compute "if" branches that will be not taken

   get opFuture >>= \case

      -- Beta reduction
      
      Abs e -> case args of
                  [] -> return ea
                  [x] -> do
                     reduceNode ctx =<< shiftReplace 0 x e
                  x:xs -> do
                     op'' <- shiftReplace 0 x e
                     reduceExpr ctx $ App op'' xs

      -- Application composition
   
      App op2 args2 -> reduceExpr ctx $ App op2 (args2 ++ args)

      -- Binary operations

      Symbol "+" -> evalBinOp ea ctx args $ \case
            [ConstInteger x, ConstInteger y] -> return (ConstInteger (x+y))
            a -> error ("Do not know how to add this: " ++ show a)

      Symbol "-" -> evalBinOp ea ctx args $ \case
            [ConstInteger x, ConstInteger y] -> return (ConstInteger (x-y))
            a -> error ("Do not know how to subtract this: " ++ show a)

      Symbol "*" -> evalBinOp ea ctx args $ \case
            [ConstInteger x, ConstInteger y] -> return (ConstInteger (x*y))
            a -> error ("Do not know how to multiply this: " ++ show a)

      Symbol "==" -> evalBinOp ea ctx args $ \case
            [ConstInteger x, ConstInteger y] -> return (ConstBool (x == y))
            a -> error ("Do not know how to compare this: " ++ show a)

      Symbol "/=" -> evalBinOp ea ctx args $ \case
            [ConstInteger x, ConstInteger y] -> return (ConstBool (x /= y))
            a -> error ("Do not know how to compare this: " ++ show a)

      -- Conditions

      Symbol "if" | length args == 3 -> do
            let [cond,thn,els] = args
            cond' <- reduceNode ctx cond
            case cond' of
               ConstBool True -> reduceNode ctx thn
               ConstBool False -> reduceNode ctx els
               _ -> error "If condition does not evaluate to a boolean"

      -- Kernel execution
      Symbol s -> do
            -- TODO: kernel launching
            args' <- reduceNodes ctx args
            putStrLn (printf "Submit task %s with args %s then wait" s (show args'))
            threadDelay =<< ((`mod` 1000000) <$> randomIO)
            return (Data 999)

      _ -> return ea

reduceExpr _ e = return e

evalBinOp :: Expr -> Map String Node -> [Node] -> ([Expr] -> IO Expr) -> IO Expr
evalBinOp ea ctx args f = if length args /= 2 
   then return ea
   else f =<< reduceNodes ctx args

-- | Reduce a list of nodes in parallel (blocking)
reduceNodes :: Map String Node -> [Node] -> IO [Expr]
reduceNodes ctx nodes = do
   nodes' <- forM nodes (forkPromise . reduceNode ctx)
   forM nodes' get

-- | Replace Var with given Expr and shift all other Vars
shiftReplace :: Int -> Node -> Node -> IO Node
shiftReplace i n e = case getNodeExpr e of
   (Var j) | i == j  -> return n
   (Var j) | j > i   -> newNodeIO (Var (j-1))
   (Var j) | j < i   -> newNodeIO (Var j)
   (Abs body)        -> newNodeIO =<< Abs <$> shiftReplace (i+1) n body
   (App op args)     -> do
                           op' <- shiftReplace i n op
                           args' <- forM args (shiftReplace i n)
                           newNodeIO (App op' args')
   _                 -> return e


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

