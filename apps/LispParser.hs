{-# LANGUAGE TupleSections, LambdaCase #-}
import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer

import Control.Monad (forM)
import Control.Applicative ( (<$>) )
import Data.Map as Map
import System.Random
import Text.Printf
import System.Environment

import Paths_ViperVM

main :: IO ()
main = do
   let file = "apps/samples/lisp/Sample.lisp"
   
       kernels = [
         makeKernel "potrf" 1,
         makeKernel "trsm" 2,
         makeKernel "syrk" 2,
         makeKernel "sgemm" 3
         ] 

       f name ags = do
          dataId <- (`mod` 1000) <$> randomIO
          putStrLn (printf "Execute kernel %s with args %s, result in #%d" name (show ags) dataId)
          return (Data dataId)
         
       makeKernel name arity = Kernel name arity (f name)
                              
   
   kerCtx <- Map.fromList <$> forM kernels (\k@(Kernel name _ _) -> (name,) <$> newNodeIO k)
   ctx1 <- readModule =<< readFile =<< getDataFileName file

   let ctx = Map.union ctx1 kerCtx
       ch  = check ctx

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
         ch "(deepSeq (map (+ 1) '(1 2 3 4)))"
         ch "(constant)"
         ch "(trimatrix3)"
         ch "(deepSeq (triangularize matrix2))"
         ch "(deepSeq (zipWith + '(1 2 3 4) '(7 7 7 7)))"
         ch "(deepSeq (zipWith2D + matrix2 matrix2))"
         ch "(deepSeq (crossWith * '(1 2 3 4) '(1 2 3)))"
         ch "(reclet 10 0)"

         ch "(deepSeq (cholesky trimatrix3))"
         ch "(deepSeq (cholRec trimatrix3))"
         ch "(deepSeq (cholRec trimatrix4))"
      _ -> error "Invalid parameters"


check :: Map String Node -> String -> IO ()
check ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show expr)
      f <- run ctx r
      putStrLn ("Reduction result: " ++ show f)
