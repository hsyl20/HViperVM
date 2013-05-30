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


   res <- getNodeExprIO a >>= \case

         Alias a1 -> atomically $ do
               a1' <- followAlias a1
               setNodeExpr a =<< getNodeExpr a1'
               return (a1:as)

         App a1 _ -> return (a1:spine)

         -- Force deep evaluation
         Symbol "deepSeq" -> reduceSpine ctx spine [True] $ \case
               -- Apply deepSeq recursively
               ([List xs],_) -> List <$> (runParallel ctx =<< traverse (newNodeIO . App a) xs)
               (_,[node]) -> return (Alias node)
               _ -> error "deepSeq error that should never be triggered"

         Symbol name 
            | Map.member name ctx -> do
                  a1 <- instantiateIO ctx (ctx Map.! name)
                  return (a1:as)

            | Map.member name builtins -> do
                  let builtin = builtins Map.! name
                  reduceSpine ctx spine (evals builtin) (action builtin)

            | otherwise -> error ("Not in scope: `" ++ show name)


         Lambda [] body -> reduceSpine ctx spine [] $ \case
               (_,_) -> return (Alias body)

         Lambda names body -> reduceSpine ctx spine (replicate (length names) False) $ \case
               (_,args) -> do
                  let ctx2 = Map.fromList (names `zip` args)
                  Alias <$> instantiateIO ctx2 body


         -- Kernel execution
         Kernel _ arity f -> reduceSpine ctx spine (replicate arity True) $ \case
               (args,_) -> f args

         e -> error ("Cannot apply " ++ show e)

   when debug (putStrLn (" |-> " ++ showSpine res))

   return res


-- | Perform a reduction on the spine and update the parent node
-- Return the new spine
reduceSpine :: Map String Node -> [Node] -> [Bool] -> (([Expr],[Node]) -> IO Expr) -> IO [Node]
reduceSpine ctx spine evalMasks f = do
   let arity = length evalMasks
       spine' = drop arity spine
       parent = head spine'
   args <- atomically (getArgs arity (tail spine))
   evalArgs <- atomically . mapM getNodeExpr =<< runParallel ctx (mask evalMasks args)
   result <- f (evalArgs,args)
   setNodeExprIO parent result
   return spine'

               
-- | Return the given number of arguments from the spine
getArgs :: Int -> [Node] -> STM [Node]
getArgs count spine = getArgs' count spine
   where
      getArgs' 0 _ = return []
      getArgs' _ [] = error (printf "Cannot retrieve %d args from the given spine" count)
      getArgs' n (x:xs) = do
         x' <- followAlias x

         getNodeExpr x' >>= \case
            (App _ a2) -> do
               arg <- followAlias a2
               (arg:) <$> getArgs' (n-1) xs

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

