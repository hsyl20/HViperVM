import ViperVM.Parsing.Lisp
import ViperVM.Reducer.Graph

import Data.Map

sample1 :: String
sample1 = "(defun f (a b c d)\
          \     \"Sum of multiplications\"\
          \     (+ (* a b) (* c d)))\
          \ \
          \(defun recf (a n)\
          \     \"Recursive function\"\
          \     (if (/= n 0)\
          \           (+ a (recf a (- n 1)))\
          \           0))"



main :: IO ()
main = do
   s1 <- readModule sample1
   putStrLn ("Parsed module: " ++ show s1)

   let ch = check s1

   ch "(+ (f 5 1 4 3) (f 2 1 3 1))"
   ch "(recf 5 10)"



check :: Map String Node -> String -> IO ()
check ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show r)
      f <- reduceNode ctx r
      putStrLn ("Reduction result: " ++ show f)
