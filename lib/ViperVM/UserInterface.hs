module ViperVM.UserInterface where

import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
import qualified ViperVM.Parsing.Lisp as Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import Data.Map as Map
import Control.Applicative ( (<$>) )

-- | Parse and evaluate a Lisp expression
evalLisp :: Map Name Builtin -> String -> IO SharedObject
evalLisp builtins expr = readData <$> (eval builtins =<< Lisp.readExpr expr)
