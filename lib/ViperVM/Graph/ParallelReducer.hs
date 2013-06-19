{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Graph.ParallelReducer (
   run, eval
) where

import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins

import Data.Map as Map
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when,void)
import Control.Applicative
import Text.Printf
import Data.Traversable (traverse)
import Data.List (intersperse)
import ViperVM.Platform.Event

debug :: Bool
debug = False

parallel :: Bool
parallel = True

future :: IO a -> IO (Event a)
future f = withNewEvent (\ev -> void (forkIO (setEvent ev =<< f)))
   

-- | Evaluate a node and return its expression
eval :: Map Name Builtin -> Map String Node -> Node -> IO Expr
eval builtins ctx node = getNodeExprIO =<< run builtins ctx node

-- | Perform several node reductions in parallel
runParallel :: Map Name Builtin -> Map String Node -> [Node] -> IO [Node]
runParallel _ _ [] = return []
runParallel builtins ctx nodes@(x:xs)= do
   if not parallel
      then traverse (run builtins ctx) nodes
      else do
         -- Spare one thread: the current one evaluates the first node
         events <- traverse (future . run builtins ctx) xs
         x' <- run builtins ctx x
         xs' <- traverse waitEvent events
         return (x':xs')

-- | Reduce a node
run :: Map Name Builtin -> Map Name Node -> Node -> IO Node
run builtins initCtx node = do
   when debug (putStrLn ("Red: " ++ showSpine [node]))

   atomically (lock node)

   -- Reduce the node
   [res] <- iterateUntilM'' (atomically . isFinal) (step initCtx) [node]

   atomically (unlock node)

   when debug (putStrLn (" |=> " ++ showSpine [res]))

   return res

   where
         
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
               Symbol "deepseq" -> reduceSpine ctx spine [True] $ \case
                     -- Apply deepseq recursively
                     ([List xs],_) -> List <$> (runParallel builtins ctx =<< traverse (newNodeIO . App a) xs)
                     (_,[n]) -> return (Alias n)
                     _ -> error "deepseq error that should never be triggered"

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
         args' <- runParallel builtins ctx (mask evalMasks args)
         evalArgs <- atomically (mapM getNodeExpr args')
         result <- f (evalArgs,args)
         setNodeExprIO parent result
         return spine'

               
-- | Return the given number of arguments from the spine
getArgs :: Int -> [Node] -> STM [Node]
getArgs count spine = do

   getArgs' count spine

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

