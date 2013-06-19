{-# LANGUAGE TupleSections, LambdaCase #-}
import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import Control.Applicative ( (<$>) )
import Data.Map as Map
import System.Random
import Text.Printf
import System.Environment
import Data.Dynamic

import Paths_ViperVM

main :: IO ()
main = do
   let file = "apps/samples/lisp/Sample.lisp"
   
       kernels = Map.fromList [
         ("potrf", Builtin [True] $ \case
            (args,_) -> f "potrf" args),

         ("trsm", Builtin [True,True] $ \case
            (args,_) -> f "trsm" args),

         ("syrk", Builtin [True,True] $ \case
            (args,_) -> f "syrk" args),

         ("sgemm", Builtin [True,True,True] $ \case
            (args,_) -> f "sgemm" args)

         ] 

       f name ags = do
          dataId <- (`mod` 1000) <$> randomIO :: IO Int
          putStrLn (printf "Execute kernel %s with args %s, result in #%d" name (show ags) dataId)
          return (Data $ toDyn dataId)
                              
   
   ctx <- readModule =<< readFile =<< getDataFileName file

   let ch  = check builtins ctx
       builtins = Map.union defaultBuiltins kernels

   getArgs >>= \case
      ["-e",s] -> ch s
      [] -> do
         ch "(+ 5 9)"
         ch "(* 5 9)"
         ch "(- 5 9)"
         ch "(- (+ 10 5) 9)"
         ch "(+ (f 5 1 4 3) (f 2 1 3 1))"
         ch "(letsgo 10 5)"
         ch "(letsgo* 10 5)"
         ch "(rec 5 10)"
         ch "(recter 5 10 0)"
         ch "(recf - 1000 5 10)"
         ch "(sum '(1 2 3 4 5))"
         ch "(sum '((+ 1 2) 3 4 5))"
         ch "(map (+ 1) '(1 2 3 4))"
         ch "(deepseq (map (+ 1) '(1 2 3 4)))"
         ch "(constant)"
         ch "(trimatrix3)"
         ch "(deepseq (triangularize matrix2))"
         ch "(deepseq (zipWith + '(1 2 3 4) '(7 7 7 7)))"
         ch "(deepseq (zipWith2D + matrix2 matrix2))"
         ch "(deepseq (crossWith * '(1 2 3 4) '(1 2 3)))"
         ch "(reclet 10 0)"

         ch "(deepseq (cholesky trimatrix3))"
         ch "(deepseq (cholRec trimatrix3))"
         ch "(deepseq (cholRec trimatrix4))"
      _ -> error "Invalid parameters"


check :: Map String Builtin -> Map String Node -> String -> IO ()
check builtins ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show expr)
      f <- run builtins ctx r
      putStrLn ("Reduction result: " ++ show f)
