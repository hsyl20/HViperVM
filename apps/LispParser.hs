import ViperVM.Parsing.Lisp

main :: IO ()
main = do
   putStrLn $ readExpr "(defun f (a b c d)\
                        \   (+ (* a b) (* c d)))"
