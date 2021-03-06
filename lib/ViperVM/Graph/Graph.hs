{-# LANGUAGE LambdaCase, TupleSections, ExistentialQuantification #-}

-- | Graph reduction module
module ViperVM.Graph.Graph (
   Node, Expr(..), Name,
   newNodeIO, newNode, followAlias,
   getNodeExpr, getNodeExprIO, setNodeExpr, setNodeExprIO, lock, unlock,
   instantiate, instantiateIO, freeVars, lambdaLift
) where

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (foldM)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Map as Map
import Data.Dynamic
import System.IO.Unsafe

-- | A node in the graph that is to be reduced.
-- Nodes are mutable by using shared transactional memory
data Node = Node (TVar Expr) (TVar Lock)
            
-- | A functional expression that may involve other nodes
data Expr = Symbol Name                               -- ^ A symbol (function name, variable name...)
          | App Node Node                             -- ^ An application
          | Lambda [Name] Node                        -- ^ A lambda abstraction
          | ConstInteger Integer                      -- ^ An integer constant
          | ConstBool Bool                            -- ^ A boolean constant
          | ListCons Node Node                        -- ^ List CONS
          | ListNil                                   -- ^ Empty list
          | Data Dynamic                              -- ^ An opaque reference to a data
          | Alias Node                                -- ^ An indirection to another node
          | Let Bool (Map Name Node) Node             -- ^ A let-expression (first arg is true if recursive let)

type Name = String            -- ^ Name for symbols
data Lock = Locked | Unlocked -- ^ Indicate if a node is locked (being reduced) or not


instance Show Expr where
   show (Symbol s) = s
   show (App op arg) = "(" ++ show op ++ " " ++ show arg ++ ")"
   show (Data i) = "#" ++ show i
   show (Lambda names body) = if Prelude.null names then show body else "(λ. " ++ concat (intersperse " " names) ++ " -> " ++ show body ++ ")"
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (ListCons x xs) = show x ++ " : " ++ show xs
   show ListNil = "[]"
   show (Alias n) = show n
   show (Let False bindings body) = "(let " ++ show (Map.toList bindings) ++ " " ++ show body ++ ")"
   show (Let True bindings body) = "(let* " ++ show (Map.toList bindings) ++ " " ++ show body ++ ")"

instance Show Node where
   show node = show $ unsafePerformIO (atomically (getNodeExpr node))

-- | Create a new node
newNode :: Expr -> STM Node
newNode e = Node <$> newTVar e <*> newTVar Unlocked

-- | IO version of newNode
newNodeIO :: Expr -> IO Node
newNodeIO = atomically . newNode

-- | Return the functional expression associated with a node
getNodeExpr :: Node -> STM Expr
getNodeExpr (Node e _) = readTVar e

-- | IO version of getNodeExpr
getNodeExprIO :: Node -> IO Expr
getNodeExprIO node = atomically (getNodeExpr node)

-- | Set the expression associated with a node
setNodeExpr :: Node -> Expr -> STM ()
setNodeExpr (Node e _) ex = writeTVar e ex

-- | IO version of setNodeExpr
setNodeExprIO :: Node -> Expr -> IO ()
setNodeExprIO node ex = atomically (setNodeExpr node ex)

-- | Lock a node or block until it is unlocked
lock :: Node -> STM ()
lock (Node _ lck) = do
   readTVar lck >>= \case
      Locked -> retry
      Unlocked -> writeTVar lck Locked

-- | Unlock a node
unlock :: Node -> STM ()
unlock (Node _ lck) = do
   readTVar lck >>= \case
      Locked -> writeTVar lck Unlocked
      Unlocked -> error "Unlocking a non locked node"


-- | Follow node aliases until another kind of node is found and returned
followAlias :: Node -> STM Node
followAlias node = getNodeExpr node >>= \case
   Alias e -> followAlias e
   _       -> return node


-- | IO version of instantiate
instantiateIO :: Map Name Node -> Node -> IO Node
instantiateIO ctx node = atomically (instantiate ctx node)

-- | Create a new instance of a node where some symbols have been replaced by associated nodes
instantiate :: Map Name Node -> Node -> STM Node
instantiate ctx node = snd <$> instantiate' ctx node

instantiate' :: Map Name Node -> Node -> STM (Bool,Node)
instantiate' ctx node = getNodeExpr node >>= \case
   ConstInteger _ -> return (False,node)
   ConstBool _    -> return (False,node)
   Data _         -> return (False,node)
   Alias e        -> instantiate' ctx e
   ListNil        -> return (False,node)
   ListCons x xs        -> do
         xs' <- instantiate' ctx xs
         x' <- instantiate' ctx x
         if or [fst x',fst xs']
            then (True,) <$> newNode (ListCons (snd x') (snd xs'))
            else return (False,node)

   Lambda names body -> do
         let ctx2 = Prelude.foldl (flip Map.delete) ctx names
         (cond,body') <- instantiate' ctx2 body
         if cond
            then (True,) <$> newNode (Lambda names body')
            else return (False,node)
     
   Symbol name 
      | Map.member name ctx -> return (True, ctx Map.! name)
      | otherwise           -> return (False,node)

   App e1 e2 -> do
         (c1,a1) <- instantiate' ctx e1
         (c2,a2) <- instantiate' ctx e2
         if c1 || c2
            then (True,) <$> newNode (App a1 a2)
            else return (False,node)

   Let False bindings body -> do
         bindings'' <- traverse (instantiate' ctx) bindings
         let cs = fmap fst (elems bindings'')
             bindings' = fmap snd bindings''
             ctx2 = Map.union ctx bindings'
         (c,body') <- instantiate' ctx2 body
         if or (c:cs)
            then return (True, body')
            else return (False,node)

   Let True bindings body -> do
         -- Placeholder nodes
         nodes <- traverse (\ _ -> newNode (ConstInteger 666)) bindings

         -- Instantiate each let body
         let ctx2 = Map.union ctx nodes
         bindings'' <- traverse (instantiate' ctx2) bindings

         -- cs indicates bodies that have been instantiated
         let cs = fmap fst (elems bindings'')
             bindings' = fmap snd bindings''
             nodeExprs = Map.intersectionWith (\n1 n2 -> (n1, Alias n2)) nodes bindings'

         -- Set real expressions to placeholder nodes
         traverse_ (uncurry setNodeExpr) nodeExprs

         -- Instantiate body
         (c,body') <- instantiate' ctx2 body
         if or (c:cs)
            then return (True, body')
            else return (False,node)


-- | Return free variables of the given node
freeVars :: Node -> STM [Name]
freeVars = freeVars' [] 
   where
      freeVars' names node = getNodeExpr node >>= \case
         Lambda ns body -> freeVars' (ns++names) body
         Symbol name
            | elem name names -> return []
            | otherwise       -> return [name]
         App e1 e2 -> (++) <$> freeVars' names e1 <*> freeVars' names e2
         ListCons e1 e2 -> (++) <$> freeVars' names e1 <*> freeVars' names e2

         Let False ctx e -> do
            let names' = names ++ keys ctx
            ctxVars <- concat <$> traverse (freeVars' names) (elems ctx)
            (++ ctxVars) <$> freeVars' names' e

         Let True ctx e -> do
            let names' = names ++ keys ctx
            ctxVars <- concat <$> traverse (freeVars' names') (elems ctx)
            (++ ctxVars) <$> freeVars' names' e

         Alias e -> freeVars' names e

         _ -> return []
   

-- | Perform lambda-lifting on node
lambdaLift :: Map Name Node -> Int -> Node -> STM (Map Name Node, Int, Node)
lambdaLift ctx idx node = getNodeExpr node >>= \case

   App e1 e2 -> do
      (ctx1, idx1, e1') <- lambdaLift ctx idx e1
      (ctx2, idx2, e2') <- lambdaLift ctx idx1 e2
      node' <- newNode (App e1' e2')
      return (union ctx1 ctx2, idx2, node')

   Lambda vars body -> do
      freeVars body >>= \case
         [] -> freeVars node >>= \case
            [] -> do
               let name = "_lambda" ++ show idx
                   ctx' = insert name node ctx
               node' <- newNode (Symbol name)
               return (ctx', idx+1, node')
            xs -> do
               lifted <- newNode (Lambda (xs++vars) body)
               let name = "_lambda" ++ show idx
                   ctx' = insert name lifted ctx
                   f ns n = do
                     s <- newNode (Symbol n)
                     newNode (App ns s)
               nameNode <- newNode (Symbol name)
               node' <- foldM f nameNode xs
               return (ctx', idx+1, node')
         _ -> do
            dummys <- Map.fromList <$> mapM (\x -> (x,) <$> newNode (Symbol "dummy")) vars
            (ctx',idx',body') <- lambdaLift (Map.union ctx dummys) idx body
            node' <- newNode (Lambda vars body')
            lambdaLift ctx' idx' node'


   ListCons e1 e2 -> do
      (ctx1, idx1, e1') <- lambdaLift ctx idx e1
      (ctx2, idx2, e2') <- lambdaLift ctx idx1 e2
      node' <- newNode (ListCons e1' e2')
      return (union ctx1 ctx2, idx2, node')

   Alias e -> do
      (ctx', idx', e') <- lambdaLift ctx idx e
      node' <- newNode (Alias e')
      return (ctx', idx', node')

   _ -> return (ctx, idx, node)

