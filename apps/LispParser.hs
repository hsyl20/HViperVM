{-# LANGUAGE TupleSections #-}
import ViperVM.Parsing.Lisp
import ViperVM.Reducer.Graph

import Control.Monad (forM)
import Control.Applicative ( (<$>) )
import Data.Map as Map

import Paths_ViperVM

main :: IO ()
main = do
   let f = "apps/samples/lisp/Sample.lisp"
   
   s1 <- readModule =<< readFile =<< getDataFileName f
   putStrLn ("Parsed module: " ++ show s1)

   let ch = check s1

   ch "(+ 5 9)"
   ch "(* 5 9)"
   ch "(- 5 9)"
   ch "(- (+ 10 5) 9)"
   ch "(+ (f 5 1 4 3) (f 2 1 3 1))"
   ch "(letsgo 10 5)"
   ch "(letsgo* 10 5)"
   ch "(rec 5 10)"
   ch "(recf - 1000 5 10)"
   ch "(sum '(1 2 3 4 5))"
   ch "(sum '((+ 1 2) 3 4 5))"
   ch "(map (+ 1) '(1 2 3 4))"
   ch "(constant)"
   ch "(matrix1)"

   kernels <- forM [Kernel "potrf" 1, Kernel "trsm" 2, Kernel "syrk" 2, Kernel "sgemm" 3] $ (\k@(Kernel name _) -> (name,) <$> newNodeIO k)

   let ctx2 = Map.union s1 (Map.fromList kernels)

   check ctx2 "(cholesky matrix1)"


check :: Map String Node -> String -> IO ()
check ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show r)
      f <- reduceNode ctx r
      putStrLn ("Reduction result: " ++ show f)
