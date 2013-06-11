module ViperVM.UserInterface (
      evalLisp, initFloatMatrix, printFloatMatrix
   ) where

import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
import qualified ViperVM.Parsing.Lisp as Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins
import ViperVM.Platform.Primitive as Prim

import Data.Map as Map
import Control.Applicative ( (<$>) )
import Data.Foldable (traverse_)

-- | Parse and evaluate a Lisp expression
evalLisp :: Map Name Builtin -> String -> IO SharedObject
evalLisp builtins expr = readData <$> (eval builtins =<< Lisp.readExpr expr)

-- | Allocate and initialize a matrix of floats
initFloatMatrix :: Runtime -> [[Float]] -> IO SharedObject
initFloatMatrix rt ds = pokeFloatMatrix rt desc ds
   where
      desc = MatrixDesc Prim.Float w h
      w = fromIntegral (length (head ds))
      h = fromIntegral (length ds)

-- | Print a float matrix on the standard output
printFloatMatrix :: Runtime -> SharedObject -> IO ()
printFloatMatrix rt so = do
   r <- peekFloatMatrix rt so
   traverse_ (putStrLn . show) r
