{-# LANGUAGE LambdaCase, TupleSections #-}
module ViperVM.Reducer.Graph (
   run, Node, Expr(..), newNodeIO
) where

import Control.Concurrent.STM
import Control.Monad ((<=<),void)
import Control.Concurrent.Future
import Control.Applicative
import Data.Traversable (forM, traverse)
import Data.List (intersperse)
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

data Lock = Locked | Unlocked

data Node = Node (TVar Expr) (TVar Lock)
            
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
          | Kernel String Int ([Node] -> IO Node)
          | Alias Node
          | Let Bool (Map Name Node) Node

instance Show Expr where
   show (Symbol s) = s
   show (App op arg) = "(" ++ show op ++ " " ++ show arg ++ ")"
   show (Data i) = "#" ++ show i
   show (Lambda names body) = if Prelude.null names then show body else "λ. " ++ concat (intersperse " " names) ++ " -> " ++ show body
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (List i) = show i
   show (Kernel n _ _) = show ("Kernel " ++ n)
   show (Alias n) = show n
   show (Let False bindings body) = "(let " ++ show (Map.toList bindings) ++ " " ++ show body ++ ")"
   show (Let True bindings body) = "(let* " ++ show (Map.toList bindings) ++ " " ++ show body ++ ")"

isDataNode :: Node -> STM Bool
isDataNode node = getNodeExpr node >>= \case
   Data _         -> return True
   ConstInteger _ -> return True
   ConstBool _    -> return True
   List _        -> return True
   _              -> return False

newNode :: Expr -> STM Node
newNode e = Node <$> newTVar e <*> newTVar Unlocked

newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

getNodeExpr :: Node -> STM Expr
getNodeExpr (Node e _) = readTVar e

getNodeExprIO :: Node -> IO Expr
getNodeExprIO node = atomically $ getNodeExpr node

setNodeExpr :: Node -> Expr -> STM ()
setNodeExpr (Node e _) ex = writeTVar e ex

lock :: Node -> IO ()
lock (Node _ lck) = atomically $ do
   readTVar lck >>= \case
      Locked -> retry
      Unlocked -> writeTVar lck Locked

unlock :: Node -> IO ()
unlock (Node _ lck) = atomically $ do
   readTVar lck >>= \case
      Locked -> writeTVar lck Unlocked
      Unlocked -> error "Unlocking a non locked node"

run :: Map Name Node -> Node -> IO Node
run ctx spine = do
   lock spine
   [res] <- iterateUntilM'' (atomically . isFinal) (step ctx) [spine]
   unlock spine
   return res
         
isFinal :: [Node] -> STM Bool
isFinal [] = error "Evaluation spine is empty"
isFinal [x] = isDataNode x
isFinal _ = return False

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
               
runParallel :: Map String Node -> [Node] -> IO [Node]
runParallel ctx = traverse get <=< traverse (forkPromise . run ctx)

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
   Kernel {}      -> return node
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
         -- Remove overloaded names from ctx
         let ctx2 = Map.difference ctx bindings
         bindings' <- traverse (instantiate ctx) bindings
         body' <- instantiate ctx2 body
         newNode (Let False bindings' body')
   Let True bindings body -> do
         -- Remove overloaded names from ctx
         let ctx2 = Map.difference ctx bindings
         bindings' <- traverse (instantiate ctx2) bindings
         body' <- instantiate ctx2 body
         newNode (Let True bindings' body')

