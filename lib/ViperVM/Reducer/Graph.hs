{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Reducer.Graph (
   run, Node, Expr(..), newNodeIO
) where

import Control.Concurrent.STM
import Control.Monad (forM_,(<=<))
import Control.Concurrent.Future
import Control.Applicative
import Control.Concurrent
import Control.Monad.Loops (allM)
import Data.Traversable (forM, traverse)
import Data.List (intersperse)
import System.Random
import Text.Printf
import Data.Map as Map
import System.IO.Unsafe

iterateUntilM'' :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM'' p f v = do
   prd <- p v
   if prd
      then return v
      else f v >>= iterateUntilM'' p f

type DataId = Int

data Node = Node (TVar Expr)
            
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
   show (Let False bindings body) = "(let " ++ show bindings ++ " " ++ show body ++ ")"
   show (Let True bindings body) = "(let* " ++ show bindings ++ " " ++ show body ++ ")"

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

   -- Perform STM operation if possible
   res <- atomically $ do
      getNodeExpr a >>= \case

         Alias a1  -> do
            a1' <- followAlias a1
            return (Just (a1':as))

         App a1 _ -> return (Just (a1:a:as))

         Symbol name 
            | Map.member name ctx -> return (Just (ctx Map.! name : as))
            | otherwise           -> return Nothing

         Lambda names body -> do
            args <- getArgs (length names) as
            let ctx2 = (Map.fromList (names `zip` args))
            inst <- instantiate ctx2 body
            return (Just (inst : drop (length names) as))

         _ -> return Nothing


   -- Perform IO operation if no STM action took place and if possible
   case res of
      Just e -> return e
      Nothing -> getNodeExprIO a >>= \case
         -- Binary operations

         Symbol "+" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstInteger (x+y)
               e -> error ("Do not know how to add this: " ++ show e)

         Symbol "-" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstInteger (x-y)
               e -> error ("Do not know how to subtract this: " ++ show e)

         Symbol "*" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstInteger (x*y)
               e -> error ("Do not know how to multiply this: " ++ show e)

         Symbol "==" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x == y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol "/=" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x /= y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol ">" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x > y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol "<" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x < y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol ">=" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x >= y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol "<=" -> evalOp ctx as 2 $ \case
               [ConstInteger x, ConstInteger y] -> ConstBool (x <= y)
               e -> error ("Do not know how to compare this: " ++ show e)

         Symbol "List.head" -> evalOp ctx as 1 $ \case
               [List []] -> error ("List.head applied to an empty list")
               [List (x:_)] -> Alias x
               e -> error ("List.head can only be applied to a list (found " ++ show e ++")")

         Symbol "List.tail" -> evalOp ctx as 1 $ \case
               [List (_:xs)] -> (List xs)
               e -> error ("List.tail can only be applied to a list (found " ++ show e ++")")

         Symbol "List.null" -> evalOp ctx as 1 $ \case
               [List xs] -> ConstBool (Prelude.null xs)
               e -> error ("List.null can only be applied to a list (found " ++ show e ++")")

         Symbol "List.cons" -> do
               args <- atomically $ getArgs 2 as
               evalOp ctx (tail as) 1 $ \case
                  [List xs] -> List (head args : xs)
                  e -> error ("List.cons can only be applied to a list (found " ++ show e ++")")

         -- Conditions

         Symbol "if" -> do
               [cond,thn,els] <- atomically $ getArgs 3 as
               cond' <- run ctx [cond] 
               atomically $ do
                  r <- getNodeExpr cond' >>= \case
                     ConstBool True -> return (Alias thn)
                     ConstBool False -> return (Alias els)
                     e -> error ("If condition does not evaluate to a boolean: " ++ show e)
                  let p = drop 2 as
                  setNodeExpr (head p) r
                  return p

         Symbol name -> error ("Not in scope: `" ++ show name)

         -- Evaluate list values
         List xs -> do
            xs' <- runParallel ctx xs
            a' <- newNodeIO (List xs') 
            return (a':as)

         -- Kernel execution
         Kernel name arity -> do
               let f ags = do
                     -- TODO: kernel launching
                     putStrLn (printf "Submit task %s with args %s then wait" name (show ags))
                     threadDelay =<< ((`mod` 100000) <$> randomIO)
                     return (Data 999)
                     
               args <- atomically $ getArgs arity as
               args' <- forM args (run ctx . pure)
               r <- f args'
               atomically $ do
                  let p = drop (arity - 1) as
                  setNodeExpr (head p) r
                  return p

         e -> error ("Cannot apply " ++ show e)
               
runParallel :: Map String Node -> [Node] -> IO [Node]
runParallel ctx = traverse get <=< traverse (forkPromise . run ctx . pure)

getArgs :: Int -> [Node] -> STM [Node]
getArgs 0 _ = return []
getArgs n [] = error (printf "Trying to get %d args, but the spine is empty" n)
getArgs n (x:xs) = do
   x' <- followAlias x

   getNodeExpr x' >>= \case
      (App _ a2) -> do
         arg <- followAlias a2
         (arg:) <$> getArgs (n-1) xs

      -- The node has been updated and is no longer an App
      _ -> retry -- FIXME: correctly handle concurrent updates


followAlias :: Node -> STM Node
followAlias node = getNodeExpr node >>= \case
   Alias e -> followAlias e
   _       -> return node

evalOp :: Map String Node -> [Node] -> Int -> ([Expr] -> Expr) -> IO [Node]
evalOp ctx as arity f = do
   let p = drop (arity - 1) as
       parent = head p
   args <- atomically (getArgs arity as)
   args' <- runParallel ctx args
   atomically $ do
      args'' <- mapM getNodeExpr args'
      let r = f args''
      setNodeExpr parent r
      return p

instantiate :: Map Name Node -> Node -> STM Node
instantiate ctx node = getNodeExpr node >>= \case
   ConstInteger _ -> return node
   ConstBool _    -> return node
   Data _         -> return node
   Kernel _ _     -> return node
   Alias e        -> instantiate ctx e
   List xs        -> newNode =<< (List <$> forM xs (instantiate ctx))
   Lambda names body -> newNode =<< (Lambda names <$> instantiate ctx2 body)
      where
         ctx2 = Prelude.foldl (flip Map.delete) ctx names
   Symbol name 
      | Map.member name ctx -> return (ctx Map.! name)
      | otherwise           -> return node
   App e1 e2      -> do
         a1 <- instantiate ctx e1
         a2 <- instantiate ctx e2
         newNode (App a1 a2)
   Let False bindings body -> do
         bindings' <- (fmap fst bindings `zip`) <$> forM (fmap snd bindings) (instantiate ctx)
         let ctx2 = Map.union ctx (Map.fromList bindings')
         instantiate ctx2 body
   Let True bindings body -> do
         -- Create placeholder nodes
         nodes <- forM bindings (\_ -> newNode (ConstInteger 666))
         let bindings' = fmap fst bindings `zip` nodes
             ctx2 = Map.union ctx (Map.fromList bindings')
         forM_ ((fmap snd bindings) `zip` nodes) $ \(expr,nod) -> do
            setNodeExpr nod =<< (Alias <$> instantiate ctx2 expr)
         instantiate ctx2 body


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






