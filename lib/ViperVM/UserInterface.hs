module ViperVM.UserInterface (
      evalLisp, evalLispModule, evalLispWithContext,
      initFloatMatrix, printFloatMatrix, initDummyFloatMatrix,
      initFloatMatrixF
   ) where

import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
import qualified ViperVM.Parsing.Lisp as Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins
import ViperVM.Platform.Primitive as Prim

import Data.Map as Map
import Data.Word
import Control.Applicative ( (<$>) )
import Data.Foldable (traverse_)

-- | Parse and evaluate a Lisp expression
evalLisp :: Map Name Builtin -> String -> IO SharedObject
evalLisp builtins expr = readData <$> (eval builtins Map.empty =<< Lisp.readExpr expr)

-- | Parse and evaluate a Lisp module
evalLispModule :: Map Name Builtin -> String -> IO SharedObject
evalLispModule builtins src = evalLispWithContext builtins src "(main)"

-- | Parse and evaluate a Lisp expression with a module context
evalLispWithContext :: Map Name Builtin -> String -> String -> IO SharedObject
evalLispWithContext builtins src expr = do
   ctx <- Lisp.readModule src
   e <- Lisp.readExpr expr
   readData <$> eval builtins ctx e

-- | Allocate and initialize a matrix of floats
initFloatMatrix :: Runtime -> [[Float]] -> IO SharedObject
initFloatMatrix rt ds = pokeFloatMatrix rt desc ds
   where
      desc = MatrixDesc Prim.Float w h
      w = fromIntegral (length (head ds))
      h = fromIntegral (length ds)

initFloatMatrixF :: Integral a => Runtime -> (a -> a -> Float) -> a -> a -> IO SharedObject
initFloatMatrixF rt f w h = initFloatMatrix rt [[f x y | x <- [0..w-1]] | y <- [0..h-1]]

-- | Allocate and initialize a dummy matrix of floats
initDummyFloatMatrix :: Runtime -> Word64 -> Word64 -> IO SharedObject
initDummyFloatMatrix rt w h = pokeDummyFloatMatrix rt desc
   where
      desc = MatrixDesc Prim.Float w h

-- | Print a float matrix on the standard output
printFloatMatrix :: Runtime -> SharedObject -> IO ()
printFloatMatrix rt so = do
   r <- peekFloatMatrix rt so
   traverse_ (putStrLn . show) r
