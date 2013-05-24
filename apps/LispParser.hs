import ViperVM.Parsing.Lisp
import ViperVM.Reducer.Graph

sample1 :: String
sample1 = "(defun f (a b c d)\
        \   \"Sum of multiplications\"\
        \   (+ (* a b) (* c d)))"


main :: IO ()
main = do
   s1 <- readModule sample1
   putStrLn ("Parsed module: " ++ show s1)

   r <- readExpr ("(+ (f 5 1 4 3) (f 2 1 3 1))")

   putStrLn ("Evaluating: " ++ show r)
   f <- reduceNode s1 r
   putStrLn ("Reduction result: " ++ show f)
