{-# LANGUAGE LambdaCase, TupleSections #-}

-- | Graph reduction module
module ViperVM.Graph.Graph (
   Node, Expr(..), Name,
   newNodeIO, newNode, followAlias,
   getNodeExpr, getNodeExprIO, setNodeExpr, setNodeExprIO, lock, unlock,
   instantiate, instantiateIO
) where

import Control.Concurrent.STM
import Control.Applicative
import Data.Traversable (forM, traverse)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Map as Map
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
          | List [Node]                               -- ^ A list instance
          | Data DataId                               -- ^ An opaque reference to a data
          | Alias Node                                -- ^ An indirection to another node
          | Let Bool (Map Name Node) Node             -- ^ A let-expression (first arg is true if recursive let)

type DataId = Int             -- ^ Data identifier
type Name = String            -- ^ Name for symbols
data Lock = Locked | Unlocked -- ^ Indicate if a node is locked (being reduced) or not


instance Show Expr where
   show (Symbol s) = s
   show (App op arg) = "(" ++ show op ++ " " ++ show arg ++ ")"
   show (Data i) = "#" ++ show i
   show (Lambda names body) = if Prelude.null names then show body else "(λ. " ++ concat (intersperse " " names) ++ " -> " ++ show body ++ ")"
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (List xs) = "[" ++ concat (intersperse ", " (fmap show xs)) ++ "]"
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
   List xs        -> do
         xs' <- forM xs (instantiate' ctx)
         if any id (fmap fst xs')
            then (True,) <$> newNode (List (fmap snd xs'))
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

