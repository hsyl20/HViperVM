import ViperVM.Parsing.Lisp
import ViperVM.Reducer.Graph

import Data.Map

sample1 :: String
sample1 = "(defun f (a b c d)\
          \     \"Sum of multiplications\"\
          \     (+ (* a b) (* c d)))\
          \ \
          \(defun rec (a n)\
          \     \"Recursive function\"\
          \     (if (/= n 0)\
          \           (+ a (rec a (- n 1)))\
          \           0))\
          \(defun recf (f init a n)\
          \     \"Recursive higher-order function\"\
          \     (if (/= n 0)\
          \           (f (recf f init a (- n 1)) a)\
          \           init))\
          \(defun sum (xs)\
          \     \"Sum numbers in the list\"\
          \     (if (List.null xs)\
          \           0\
          \           (+ (List.head xs) (sum (List.tail xs)))))\
          \(defun map (f xs)\
          \     \"Map f on xs\"\
          \     (if (List.null xs)\
          \          '()\
          \          (List.cons\
          \                (f (List.head xs))\
          \                (map f (List.tail xs)))))"



main :: IO ()
main = do
   s1 <- readModule sample1
   putStrLn ("Parsed module: " ++ show s1)

   let ch = check s1

   ch "(+ (f 5 1 4 3) (f 2 1 3 1))"
   ch "(rec 5 10)"
   ch "(recf - 1000 5 10)"
   ch "(sum '(1 2 3 4 5))"
   ch "(sum '((+ 1 2) 3 4 5))"
   ch "(map (+ 1) '(1 2 3 4))"



check :: Map String Node -> String -> IO ()
check ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show r)
      f <- reduceNode ctx r
      putStrLn ("Reduction result: " ++ show f)
