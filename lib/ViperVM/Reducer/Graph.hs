{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Reducer.Graph where

import Control.Concurrent.STM
import Control.Monad (forM_,void,foldM, (<=<))
import Control.Concurrent.Future
import Control.Applicative
import Control.Concurrent
import Data.Traversable (forM, traverse)
import System.Random
import Text.Printf
import Data.Map as Map
import Data.Maybe (fromMaybe,isJust)
import System.IO.Unsafe


type NodeId = Int
type DataId = Int

data Status = Inactive | Computing | Computed

data Node = Node (TVar Expr) (TVar Status)
            
instance Eq Node where
   (==) a b = unsafePerformIO $ do
                  a' <- atomically $ getNodeExpr a
                  b' <- atomically $ getNodeExpr b
                  return (a' == b')

instance Ord Node where
   compare a b = unsafePerformIO $ do
                  a' <- atomically $ getNodeExpr a
                  b' <- atomically $ getNodeExpr b
                  return (compare a' b')

instance Show Node where
   show (Node expr _) = unsafePerformIO $ do
                           e <- atomically $ readTVar expr
                           return (show e)

data Expr = Symbol String 
          | App Node [Node]
          | Data DataId
          | Var Int
          | Abs Node
          | ConstInteger Integer
          | ConstBool Bool
          | List [Node]
          | Kernel String Int
          | Alias Node
          deriving (Eq,Ord)

instance Show Expr where
   show (Symbol s) = s
   show (App op args) = "(" ++ show op ++ (concat $ fmap (\x -> " " ++ show x) args) ++ ")"
   show (Data i) = "#" ++ show i
   show (Var i) = "v" ++ show i
   show (Abs n) = "λ. " ++ show n
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (List i) = show i
   show (Kernel n _) = show ("Kernel " ++ n)
   show (Alias n) = show n


newNode :: Expr -> STM Node
newNode e = Node <$> newTVar e <*> newTVar Inactive

newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

getNodeVar :: Node -> TVar Status
getNodeVar (Node _ stat) = stat

getNodeStatus :: Node -> STM Status
getNodeStatus (Node _ stat) = readTVar stat

setNodeStatus :: Node -> Status -> STM ()
setNodeStatus (Node _ stat) s = writeTVar stat s

getNodeExpr :: Node -> STM Expr
getNodeExpr (Node e _) = readTVar e

getNodeExprIO :: Node -> IO Expr
getNodeExprIO node = atomically $ getNodeExpr node

setNodeExpr :: Node -> Expr -> STM ()
setNodeExpr (Node e _) ex = writeTVar e ex

-- | Perform graph reduction starting on the given node
reduceNode :: Map String Node -> Node -> IO Expr
reduceNode ctx node = do

   -- Check that the node is not being reduced (block in this case)
   -- Set status to Computing if this thread will handle the reduction
   -- Return the computed result
   (stat,expr) <- atomically $ do
                     getNodeStatus node >>= \case
                        Computing -> retry  -- Block if already computing
                        Inactive -> setNodeStatus node Computing >> ((Computing,) <$> getNodeExpr node)
                        Computed -> (Computed,) <$> getNodeExpr node
  
   -- Perform the reduction if appropriate
   case stat of
      Inactive -> error "Should not be inactive"
      Computed -> return expr
      Computing -> do
          --putStrLn (show expr)
          expr' <- reduceExpr ctx expr
          atomically $ do
            setNodeExpr node expr'
            setNodeStatus node Computed
          return expr'


reduceExpr :: Map String Node -> Expr -> IO Expr
reduceExpr ctx expr = case expr of

   Alias e -> reduceNode ctx e

   Symbol s | Map.member s ctx -> 
         -- Perform node substitution
         --reduceNode ctx (ctx Map.! s)
         atomically (getNodeExpr (ctx Map.! s))

   List xs -> do
         -- Deep list evaluation
         void $ reduceNodes ctx xs
         return expr

   App op [] ->
         reduceNode ctx op

   App op args -> do

      opFuture <- forkPromise (reduceNode ctx op)

      op' <- get opFuture

      -- Compute args greedily
      --case op' of
      --   Symbol "if" -> return ()
      --   _ -> void $ reduceNodes ctx args

      case op' of

         -- Beta reduction
         Abs e -> case args of
                     [] -> return expr
                     [x] -> do
                        x' <- reduceNode ctx x
                        reduceNode ctx =<< atomically (newNode =<< shiftReplace 0 x' =<< getNodeExpr e)
                     x:xs -> do
                        x' <- reduceNode ctx x
                        op'' <- atomically (newNode =<< shiftReplace 0 x' =<< getNodeExpr e)
                        reduceNode ctx =<< newNodeIO (App op'' xs)

         -- Application composition
      
         App op2 args2 -> reduceNode ctx =<< newNodeIO (App op2 (args2 ++ args))

         -- Binary operations

         Symbol "+" -> do
            putStrLn "+"
            evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstInteger (x+y)
               a -> error ("Do not know how to add this: " ++ show a)

         Symbol "-" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstInteger (x-y)
               a -> error ("Do not know how to subtract this: " ++ show a)

         Symbol "*" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstInteger (x*y)
               a -> error ("Do not know how to multiply this: " ++ show a)

         Symbol "==" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x == y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol "/=" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x /= y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol ">" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x > y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol "<" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x < y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol ">=" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x >= y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol "<=" -> evalBinOp expr ctx args $ \case
               [ConstInteger x, ConstInteger y] -> return $ ConstBool (x <= y)
               a -> error ("Do not know how to compare this: " ++ show a)

         Symbol "List.head" | length args == 1 -> 
               reduceNode ctx (head args) >>= \case
                  List [] -> error ("List.head applied to an empty list")
                  List (x:_) -> reduceNode ctx x
                  a -> error ("List.head can only be applied to a list (found " ++ show a ++")")

         Symbol "List.tail" | length args == 1 ->
               reduceNode ctx (head args) >>= \case
                  List (_:xs) -> reduceNode ctx =<< newNodeIO (List xs)
                  a -> error ("List.tail can only be applied to a list (found " ++ show a ++")")

         Symbol "List.null" | length args == 1 ->
               reduceNode ctx (head args) >>= \case
                  List xs -> return $ ConstBool (Prelude.null xs)
                  a -> error ("List.null can only be applied to a list (found " ++ show a ++")")

         Symbol "List.cons" | length args == 2 ->
               reduceNode ctx (head $ tail args) >>= \case
                  List xs -> reduceNode ctx =<< newNodeIO (List (head args : xs))
                  a -> error ("List.cons can only be applied to a list (found " ++ show a ++")")

         -- Conditions

         Symbol "if" | length args == 3 -> do
               let [cond,thn,els] = args
               reduceNode ctx cond >>= \case
                  ConstBool True -> reduceNode ctx thn
                  ConstBool False -> reduceNode ctx els
                  a -> error ("If condition does not evaluate to a boolean: " ++ show a)

         -- Kernel execution
         Kernel name arity | length args == arity -> do
               -- TODO: kernel launching
               args' <- reduceNodes ctx args
               putStrLn (printf "Submit task %s with args %s then wait" name (show args'))
               threadDelay =<< ((`mod` 100000) <$> randomIO)
               return (Data 999)

         _ -> return expr

   _ -> return expr



evalBinOp :: Expr -> Map String Node -> [Node] -> ([Expr] -> IO Expr) -> IO Expr
evalBinOp ea ctx args f = if length args /= 2 
   then return ea
   else f =<< (reduceNodes ctx args)

-- | Reduce a list of nodes in parallel (blocking)
reduceNodes :: Map String Node -> [Node] -> IO [Expr]
reduceNodes ctx nodes = do
   nodes' <- forM nodes (forkPromise . reduceNode ctx)
   forM nodes' get

-- | Replace with sharing (TODO)
shiftReplace :: Int -> Expr -> Expr -> STM Expr
shiftReplace i n e = case e of
   (Var j) | i == j  -> return n
   (Var j) | j > i   -> return (Var (j-1))
   (Var j) | j < i   -> return (Var j)
   (Abs body)        -> Abs <$> (newNode =<< shiftReplace (i+1) n =<< getNodeExpr body)
   (App op args)     -> App <$> (newNode =<< shiftReplace i n =<< getNodeExpr op) <*> forM args (newNode <=< shiftReplace i n <=< getNodeExpr)
   (Alias j)         -> Alias <$> (newNode =<< shiftReplace i n =<< getNodeExpr j)
   _                 -> return e



-- | Common sub-expression elimination
cse :: Node -> IO Node
cse node = snd <$> cseNode Map.empty node

-- | Common sub-expression elimination with a given dictionary of cse
cseNode :: Map Expr Node -> Node -> IO (Map Expr Node, Node)
cseNode exprs node = do

   e <- atomically $ getNodeExpr node

   case Map.lookup e exprs of
       Just a -> return (exprs, a)
       Nothing -> do
         (m,exs) <- cseExpr exprs e
         n <- newNodeIO exs
         let m' = insert e n m
         return (m',n)

cseExpr :: Map Expr Node -> Expr -> IO (Map Expr Node, Expr)
cseExpr exprs expr = do

       case expr of

          Abs e -> do
               (_,e') <- cseNode exprs e -- we do not return the map as it could contain some Var
               return (exprs, Abs e')

          App op args -> do
               (ns1,op') <- cseNode exprs op

               let f (ns,as) a = do
                     (ns',a') <- cseNode ns a
                     return (ns', a':as)

               (ns2,args') <- foldM f (ns1,[]) args

               return (ns2, App op' args')

          _ -> return (exprs,expr)






