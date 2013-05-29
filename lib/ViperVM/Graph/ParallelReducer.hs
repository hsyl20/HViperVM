{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Graph.ParallelReducer (
   run
) where

import ViperVM.Graph.Graph
import Data.Map as Map
import Control.Concurrent.STM
import Control.Monad ((<=<),void)
import Control.Concurrent.Future
import Control.Applicative
import Text.Printf
import Data.Traversable (forM, traverse)

-- | Reduce a node
run :: Map Name Node -> Node -> IO Node
run ctx node = do
   lock node
   [res] <- iterateUntilM'' (atomically . isFinal) (step ctx) [node]
   unlock node
   return res

-- | Indicate if the node is a data (cannot be reduced anymore)
isDataNode :: Node -> STM Bool
isDataNode node = getNodeExpr node >>= \case
   Data _         -> return True
   ConstInteger _ -> return True
   ConstBool _    -> return True
   List _        -> return True
   _              -> return False

-- | Indicate if a spine is reduced to a final value
isFinal :: [Node] -> STM Bool
isFinal [] = error "Evaluation spine is empty"
isFinal [x] = isDataNode x
isFinal _ = return False

         
-- | Perform a step on the spine (unwind or reduction)
step :: Map Name Node -> [Node] -> IO [Node]
step _ [] = error "Evaluation spine is empty"
step ctx spine@(a:as) = do

   -- Perform STM operation if possible
   res <- atomically $ do
      getNodeExpr a >>= \case

         Alias a1  -> do
            a1' <- followAlias a1
            return (Just (a1':as))

         App a1 _ -> return (Just (a1:spine))

         Symbol name 
            | Map.member name ctx -> return (Just (ctx Map.! name : as))
            | otherwise           -> return Nothing

         Lambda names body -> do
            args <- getArgs (length names) as
            let ctx2 = Map.fromList (names `zip` args)
            inst <- instantiate ctx2 body
            return (Just (inst : drop (length names) as))

         Let False bindings body -> do
            let ctx2 = Map.union ctx bindings
            inst <- instantiate ctx2 body
            return (Just (inst : as))

         Let True bindings body -> do
            -- Placeholder nodes
            bindings' <- traverse (\ _ -> newNode (ConstInteger 666)) bindings
            void $ traverseWithKey (\ name expr -> setNodeExpr (bindings' Map.! name) =<< Alias <$> instantiate bindings' expr) bindings
            let ctx2 = Map.union ctx bindings'
            inst <- instantiate ctx2 body
            return (Just (inst : as))

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

         Symbol "List.deepSeq" -> do
               arg <- run ctx =<< head <$> atomically (getArgs 1 as)
               atomically (getNodeExpr arg) >>= \case
                  -- Apply List.deepSeq recursively
                  List xs -> do
                     xs' <- runParallel ctx =<< traverse (newNodeIO . App a) xs
                     e <- newNodeIO (List xs')
                     return (e : tail as)
                  _ -> return [arg]

         -- Conditions

         Symbol "if" -> do
               [cond,thn,els] <- atomically $ getArgs 3 as
               cond' <- run ctx cond 
               atomically $ do
                  r <- getNodeExpr cond' >>= \case
                     ConstBool True -> return (Alias thn)
                     ConstBool False -> return (Alias els)
                     e -> error ("If condition does not evaluate to a boolean: " ++ show e)
                  let p = drop 2 as
                  setNodeExpr (head p) r
                  return p

         Symbol name -> error ("Not in scope: `" ++ show name)

         -- Kernel execution
         Kernel _ arity f -> do
               args <- atomically $ getArgs arity as
               args' <- forM args (run ctx)
               r <- f args'
               atomically $ do
                  let p = drop (arity - 1) as
                  setNodeExpr (head p) (Alias r)
                  return p

         e -> error ("Cannot apply " ++ show e)

-- | Evaluate a built-in operator
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

               
-- | Perform several node reductions in parallel
runParallel :: Map String Node -> [Node] -> IO [Node]
runParallel ctx = traverse get <=< traverse (forkPromise . run ctx)

-- | Return the given number of arguments from the spine
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
      _ -> retry 

-- | Helper function that iterates until a given monadic condition is met
iterateUntilM'' :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM'' p f v = do
   prd <- p v
   if prd
      then return v
      else f v >>= iterateUntilM'' p f

