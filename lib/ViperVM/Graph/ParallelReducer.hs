{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Graph.ParallelReducer (
   run
) where

import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins

import Data.Map as Map
import Control.Concurrent.STM
import Control.Monad ((<=<),void,when,liftM)
import Control.Concurrent.Future
import Control.Applicative
import Text.Printf
import Data.Traversable (traverse)
import Data.List (intersperse)

debug :: Bool
debug = False

-- | Perform several node reductions in parallel
runParallel :: Map String Node -> [Node] -> IO [Node]
runParallel ctx = do
   if debug
      then traverse (run ctx)
      else traverse get <=< traverse (forkPromise . run ctx)

-- | Reduce a node
run :: Map Name Node -> Node -> IO Node
run ctx node = do
   -- Lock alias nodes and node to reduce
   void . atomically $ do
      node' <- iterateUntilM'' (liftM not . isAlias) (\x -> lock x >> return x) node
      lock node'

   when debug (putStrLn ("Red: " ++ showSpine [node]))

   -- Reduce the node
   [res] <- iterateUntilM'' (atomically . isFinal) (step ctx) [node]
   when debug (putStrLn (" |=> " ++ showSpine [res]))

   -- Unlock alias nodes and reduced node
   void . atomically $ do
      node' <- iterateUntilM'' (liftM not . isAlias) (\x -> unlock x >> return x) node
      unlock node'

   return res
         
-- | Perform a step on the spine (unwind or reduction)
step :: Map Name Node -> [Node] -> IO [Node]
step _ [] = error "Evaluation spine is empty"
step ctx spine@(a:as) = do


   -- Perform STM operation if possible
   res <- atomically $ do
      r <- getNodeExpr a >>= \case

         Alias a1 -> do
            a1' <- followAlias a1
            setNodeExpr a =<< getNodeExpr a1'
            return (Just (a1:as))

         App a1 _ -> return (Just (a1:spine))

         Symbol name 
            | Map.member name ctx -> do
                                       let a' = ctx Map.! name
                                       return (Just (a':as))
            | otherwise           -> return Nothing

         Lambda [] body -> do
            return (Just (body:as))

         Lambda names body -> do
            args <- getArgs (length names) as
            let ctx2 = Map.fromList (names `zip` args)
                as' = drop (length names) as
            a' <- instantiate ctx2 body
            return (Just (a':as'))

         _ -> return Nothing
      return r


   -- Perform IO operation if no STM action took place and if possible
   res2 <- case res of
      Just e -> return e
      Nothing -> getNodeExprIO a >>= \case

         -- Force deep evaluation
         Symbol "deepSeq" -> reduceSpine ctx as [True] $ \case
               -- Apply List.deepSeq recursively
               ([List xs],_) -> List <$> (runParallel ctx =<< traverse (newNodeIO . App a) xs)
               (_,[node]) -> return (Alias node)
               _ -> error "deepSeq error that should never be triggered"

         -- Built-ins
         Symbol name 
            | Map.member name builtins -> do
                  let builtin = builtins Map.! name
                  reduceSpine ctx as (evals builtin) (action builtin)
            | otherwise -> error ("Not in scope: `" ++ show name)

         -- Kernel execution
         Kernel _ arity f -> reduceSpine ctx as (replicate arity True) $ \case
               (args,_) -> f args

         e -> error ("Cannot apply " ++ show e)

   when debug (putStrLn (" |-> " ++ showSpine res2))

   return res2


-- | Perform a reduction on the spine and update the parent node
-- Return the new spine
reduceSpine :: Map String Node -> [Node] -> [Bool] -> (([Expr],[Node]) -> IO Expr) -> IO [Node]
reduceSpine ctx spine evalMasks f = do
   let arity = length evalMasks
       spine' = drop (arity - 1) spine
       parent = head spine'
   args <- atomically (getArgs arity spine)
   evalArgs <- atomically . mapM getNodeExpr =<< runParallel ctx (mask evalMasks args)
   result <- f (evalArgs,args)
   setNodeExprIO parent result
   return spine'

               
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


----------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------

-- | Helper function that iterates until a given monadic condition is met
iterateUntilM'' :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM'' p f v = do
   prd <- p v
   if prd
      then return v
      else f v >>= iterateUntilM'' p f

-- | Indicate if a node is an Alias
isAlias :: Node -> STM Bool
isAlias node = getNodeExpr node >>= \case
   Alias _  -> return True
   _        -> return False


-- | Indicate if the node is a data (cannot be reduced anymore)
isDataNode :: Node -> STM Bool
isDataNode node = getNodeExpr node >>= \case
   Data _         -> return True
   ConstInteger _ -> return True
   ConstBool _    -> return True
   List _         -> return True
   _              -> return False

-- | Indicate if a spine is reduced to a final value
isFinal :: [Node] -> STM Bool
isFinal [] = error "Evaluation spine is empty"
isFinal [x] = isDataNode x
isFinal _ = return False

-- | Display a list of nodes (usually the spine)
showSpine :: [Node] -> String
showSpine xs = "" ++ concat (intersperse "  :::  " (fmap show xs)) ++ ""

-- | Use a mask list to filter a list
mask :: [Bool] -> [a] -> [a]
mask ms = fmap snd . Prelude.filter fst . zip ms

