{-# LANGUAGE LambdaCase, TupleSections #-}

-- | Graph reduction module
module ViperVM.Graph.Graph (
   Node, Expr(..), Name,
   newNodeIO, newNode, followAlias,
   getNodeExpr, getNodeExprIO, setNodeExpr, lock, unlock,
   instantiate
) where

import Control.Concurrent.STM
import Control.Applicative
import Data.Traversable (forM, traverse)
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
          | Kernel String Int ([Node] -> IO Node)     -- ^ A refernce to a kernel
          | Alias Node                                -- ^ An indirection to another node
          | Let Bool (Map Name Node) Node             -- ^ A let-expression (first arg is true if recursive let)

type DataId = Int             -- ^ Data identifier
type Name = String            -- ^ Name for symbols
data Lock = Locked | Unlocked -- ^ Indicate if a node is locked (being reduced) or not


instance Show Expr where
   show (Symbol s) = s
   show (App op arg) = "(" ++ show op ++ " " ++ show arg ++ ")"
   show (Data i) = "#" ++ show i
   show (Lambda names body) = if Prelude.null names then show body else "λ. " ++ concat (intersperse " " names) ++ " -> " ++ show body
   show (ConstInteger i) = show i
   show (ConstBool i) = show i
   show (List i) = show i
   show (Kernel n _ _) = show ("Kernel " ++ n)
   show (Alias n) = "Alias"--show n
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
getNodeExprIO node = atomically $ getNodeExpr node

-- | Set the expression associated with a node
setNodeExpr :: Node -> Expr -> STM ()
setNodeExpr (Node e _) ex = writeTVar e ex

-- | Lock a node or block until it is unlocked
lock :: Node -> IO ()
lock (Node _ lck) = atomically $ do
   readTVar lck >>= \case
      Locked -> retry
      Unlocked -> writeTVar lck Locked

-- | Unlock a node
unlock :: Node -> IO ()
unlock (Node _ lck) = atomically $ do
   readTVar lck >>= \case
      Locked -> writeTVar lck Unlocked
      Unlocked -> error "Unlocking a non locked node"


-- | Follow node aliases until another kind of node is found and returned
followAlias :: Node -> STM Node
followAlias node = getNodeExpr node >>= \case
   Alias e -> followAlias e
   _       -> return node

-- | Create a new instance of a node where some symbols have been replaced by associated nodes
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

