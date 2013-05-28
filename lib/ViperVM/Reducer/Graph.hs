{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Reducer.Graph where

import Control.Concurrent.STM
import Control.Monad (forM_,void,foldM, (<=<))
import Control.Concurrent.Future
import Control.Applicative
import Control.Concurrent
import Control.Monad.Loops (allM)
import Data.Traversable (forM, traverse)
import Data.List (intersperse)
import System.Random
import Text.Printf
import Data.Map as Map
import Data.Maybe (fromMaybe,isJust)
import System.IO.Unsafe

iterateUntilM'' :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM'' p f v = do
   pred <- p v
   if pred
      then return v
      else f v >>= iterateUntilM'' p f

type NodeId = Int
type DataId = Int

data Node = Node (TVar Expr)
            
instance Eq Node where
   (==) a b = unsafePerformIO $ atomically ((==) <$> getNodeExpr a <*> getNodeExpr b)

instance Ord Node where
   compare a b = unsafePerformIO $ atomically (compare <$> getNodeExpr a <*> getNodeExpr b)

instance Show Node where
   show node = show $ unsafePerformIO (atomically (getNodeExpr node))

type Name = String

data Expr = Symbol Name
          | App Node Node
          | Lambda [Name] Node
          | ConstInteger Integer
          | ConstBool Bool
          | List [Node]
          | Data DataId
          | Kernel String Int
          | Alias Node
          | Let Bool [(Name,Node)] Node
          deriving (Eq,Ord)

instance Show Expr where
   show (Symbol s) = s
   show (App op arg) = "(" ++ show op ++ " " ++ show arg ++ ")"
   show (Data i) = "#" ++ show i
   show (Lambda names body) = if Prelude.null names then show body else "λ. " ++ concat (intersperse " " names) ++ " -> " ++ show body
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (List i) = show i
   show (Kernel n _) = show ("Kernel " ++ n)
   show (Alias n) = show n

isDataNode :: Node -> STM Bool
isDataNode node = getNodeExpr node >>= \case
   Data _         -> return True
   ConstInteger _ -> return True
   ConstBool _    -> return True
   List xs        -> allM isDataNode xs
   _              -> return False

newNode :: Expr -> STM Node
newNode e = Node <$> newTVar e

newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

getNodeExpr :: Node -> STM Expr
getNodeExpr (Node e) = readTVar e

getNodeExprIO :: Node -> IO Expr
getNodeExprIO node = atomically $ getNodeExpr node

setNodeExpr :: Node -> Expr -> STM ()
setNodeExpr (Node e) ex = writeTVar e ex

run :: Map Name Node -> [Node] -> IO Node
run ctx spine = do
   [res] <- iterateUntilM'' (atomically . isFinal) (step ctx) spine
   return res
         
isFinal :: [Node] -> STM Bool
isFinal [] = error "Evaluation spine is empty"
isFinal [x] = isDataNode x
isFinal _ = return False

step :: Map Name Node -> [Node] -> IO [Node]
step _ [] = error "Evaluation spine is empty"
step ctx (a:as) = do

   putStrLn (show (a:as))

   res <- atomically $ do
      getNodeExpr a >>= \case

         App a1 a2 -> return (Left (a1:a:as))

         Symbol name 
            | Map.member name ctx -> return (Left (ctx Map.! name : as))
            | otherwise           -> return (Right name)

         Lambda names body -> do
            args <- getArgs (length names) as
            let ctx2 = Map.union ctx (Map.fromList (names `zip` args))
            inst <- instantiate ctx2 body
            return (Left (inst : drop (length names) as))

         e -> error ("Cannot apply " ++ show e)

   case res of
      Left e -> return e

      -- Binary operations

      Right "+" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstInteger (x+y)
            e -> error ("Do not know how to add this: " ++ show e)

      Right "-" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstInteger (x-y)
            a -> error ("Do not know how to subtract this: " ++ show a)

      Right "*" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstInteger (x*y)
            a -> error ("Do not know how to multiply this: " ++ show a)

      Right "==" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x == y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right "/=" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x /= y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right ">" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x > y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right "<" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x < y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right ">=" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x >= y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right "<=" -> evalOp ctx as 2 $ \case
            [ConstInteger x, ConstInteger y] -> ConstBool (x <= y)
            a -> error ("Do not know how to compare this: " ++ show a)

      Right "List.head" -> evalOp ctx as 1 $ \case
            [List []] -> error ("List.head applied to an empty list")
            [List (x:_)] -> Alias x
            a -> error ("List.head can only be applied to a list (found " ++ show a ++")")

      Right "List.tail" -> evalOp ctx as 1 $ \case
            [List (_:xs)] -> (List xs)
            a -> error ("List.tail can only be applied to a list (found " ++ show a ++")")

      Right "List.null" -> evalOp ctx as 1 $ \case
            [List xs] -> ConstBool (Prelude.null xs)
            a -> error ("List.null can only be applied to a list (found " ++ show a ++")")

      Right "List.cons" -> do
            args <- atomically $ getArgs 2 as
            evalOp ctx (tail as) 1 $ \case
               [List xs] -> List (head args : xs)
               a -> error ("List.cons can only be applied to a list (found " ++ show a ++")")

      -- Conditions

      Right "if" -> do
            [cond,thn,els] <- atomically $ getArgs 3 as
            cond' <- run ctx [cond] 
            atomically $ do
               r <- getNodeExpr cond' >>= \case
                  ConstBool True -> return (Alias thn)
                  ConstBool False -> return (Alias els)
                  a -> error ("If condition does not evaluate to a boolean: " ++ show a)
               let p = head (drop 2 as)
               setNodeExpr p r
               return [p]

      Right name -> error ("Not in scope: `" ++ show name)
               

getArgs :: Int -> [Node] -> STM [Node]
getArgs 0 _ = return []
getArgs n (x:xs) = (:) <$> (followAlias =<< (\(App _ a2) -> a2) <$> getNodeExpr x) <*> getArgs (n-1) xs

followAlias :: Node -> STM Node
followAlias node = getNodeExpr node >>= \case
   Alias e -> followAlias e
   _       -> return node

evalOp :: Map String Node -> [Node] -> Int -> ([Expr] -> Expr) -> IO [Node]
evalOp ctx as arity f = do
   args <- atomically $ getArgs arity as
   args' <- forM args (run ctx . pure)
   atomically $ do
      args'' <- mapM getNodeExpr args'
      let p = head (drop (arity - 1) as)
          r = f args''
      setNodeExpr p r
      return [p]

instantiate :: Map Name Node -> Node -> STM Node
instantiate ctx node = getNodeExpr node >>= \case
   ConstInteger _ -> return node
   ConstBool _    -> return node
   Data _         -> return node
   Symbol name 
      | Map.member name ctx -> return (ctx Map.! name)
      | otherwise           -> return node
   App e1 e2      -> do
                        a1 <- instantiate ctx e1
                        a2 <- instantiate ctx e2
                        newNode (App a1 a2)

---- | Perform graph reduction starting on the given node
--reduceNode :: Map String Node -> Node -> IO Expr
--reduceNode ctx node = do
--
--   -- Check that the node is not being reduced (block in this case)
--   -- Set status to Computing if this thread will handle the reduction
--   -- Return the computed result
--   (stat,expr) <- atomically $ do
--                     getNodeStatus node >>= \case
--                        Computing -> retry  -- Block if already computing
--                        Inactive -> setNodeStatus node Computing >> ((Computing,) <$> getNodeExpr node)
--                        Computed -> (Computed,) <$> getNodeExpr node
--  
--   -- Perform the reduction if appropriate
--   case stat of
--      Inactive -> error "Should not be inactive"
--      Computed -> return expr
--      Computing -> do
--          --putStrLn (show expr)
--          expr' <- reduceExpr ctx expr
--          atomically $ do
--            setNodeExpr node expr'
--            setNodeStatus node Computed
--          return expr'
--
--reduceExpr :: Map String Node -> Expr -> IO Expr
--reduceExpr ctx expr = case expr of
--
--   Alias e -> reduceNode ctx e
--
--   Symbol s | Map.member s ctx -> 
--         -- Perform node substitution
--         --reduceNode ctx (ctx Map.! s)
--         atomically (getNodeExpr (ctx Map.! s))
--
--   List xs -> do
--         -- Deep list evaluation
--         void $ reduceNodes ctx xs
--         return expr
--
--   App op [] ->
--         reduceNode ctx op
--
--   App op args -> do
--
--      opFuture <- forkPromise (reduceNode ctx op)
--
--      op' <- get opFuture
--
--      -- Compute args greedily
--      --case op' of
--      --   Symbol "if" -> return ()
--      --   _ -> void $ reduceNodes ctx args
--
--      case op' of
--
--         -- Beta reduction
--         Abs e -> case args of
--                     [] -> return expr
--                     [x] -> do
--                        x' <- reduceNode ctx x
--                        reduceNode ctx =<< atomically (newNode =<< shiftReplace 0 x' =<< getNodeExpr e)
--                     x:xs -> do
--                        x' <- reduceNode ctx x
--                        op'' <- atomically (newNode =<< shiftReplace 0 x' =<< getNodeExpr e)
--                        reduceNode ctx =<< newNodeIO (App op'' xs)
--
--         -- Application composition
--      
--         App op2 args2 -> reduceNode ctx =<< newNodeIO (App op2 (args2 ++ args))
--
--         -- Kernel execution
--         Kernel name arity | length args == arity -> do
--               -- TODO: kernel launching
--               args' <- reduceNodes ctx args
--               putStrLn (printf "Submit task %s with args %s then wait" name (show args'))
--               threadDelay =<< ((`mod` 100000) <$> randomIO)
--               return (Data 999)
--
--         _ -> return expr
--
--   _ -> return expr
--
--
--
--evalBinOp :: Expr -> Map String Node -> [Node] -> ([Expr] -> IO Expr) -> IO Expr
--evalBinOp ea ctx args f = if length args /= 2 
--   then return ea
--   else f =<< (reduceNodes ctx args)
--
---- | Reduce a list of nodes in parallel (blocking)
--reduceNodes :: Map String Node -> [Node] -> IO [Expr]
--reduceNodes ctx nodes = do
--   nodes' <- forM nodes (forkPromise . reduceNode ctx)
--   forM nodes' get
--
---- | Replace with sharing (TODO)
--shiftReplace :: Int -> Expr -> Expr -> STM Expr
--shiftReplace i n e = case e of
--   (Var j) | i == j  -> return n
--   (Var j) | j > i   -> return (Var (j-1))
--   (Var j) | j < i   -> return (Var j)
--   (Abs body)        -> Abs <$> (newNode =<< shiftReplace (i+1) n =<< getNodeExpr body)
--   (App op args)     -> App <$> (newNode =<< shiftReplace i n =<< getNodeExpr op) <*> forM args (newNode <=< shiftReplace i n <=< getNodeExpr)
--   (Alias j)         -> Alias <$> (newNode =<< shiftReplace i n =<< getNodeExpr j)
--   _                 -> return e
--
--
--
---- | Common sub-expression elimination
--cse :: Node -> IO Node
--cse node = snd <$> cseNode Map.empty node
--
---- | Common sub-expression elimination with a given dictionary of cse
--cseNode :: Map Expr Node -> Node -> IO (Map Expr Node, Node)
--cseNode exprs node = do
--
--   e <- atomically $ getNodeExpr node
--
--   case Map.lookup e exprs of
--       Just a -> return (exprs, a)
--       Nothing -> do
--         (m,exs) <- cseExpr exprs e
--         n <- newNodeIO exs
--         let m' = insert e n m
--         return (m',n)
--
--cseExpr :: Map Expr Node -> Expr -> IO (Map Expr Node, Expr)
--cseExpr exprs expr = do
--
--       case expr of
--
--          Abs e -> do
--               (_,e') <- cseNode exprs e -- we do not return the map as it could contain some Var
--               return (exprs, Abs e')
--
--          App op args -> do
--               (ns1,op') <- cseNode exprs op
--
--               let f (ns,as) a = do
--                     (ns',a') <- cseNode ns a
--                     return (ns', a':as)
--
--               (ns2,args') <- foldM f (ns1,[]) args
--
--               return (ns2, App op' args')
--
--          _ -> return (exprs,expr)






