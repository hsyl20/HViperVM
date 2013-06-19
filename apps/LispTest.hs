import ViperVM.Platform.Platform
import ViperVM.Platform.Runtime
import ViperVM.Platform.Logger
import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixMul
import ViperVM.UserInterface
import ViperVM.Scheduling.Eager

main :: IO ()
main = do

   let config = Configuration {
          libraryOpenCL = "libOpenCL.so",
          logger = stdOutLogger . filterLevel LogDebug
       }

   pf <- initPlatform config
   rt <- initRuntime pf eagerScheduler

   -- Initialize some data
   a <- initFloatMatrix rt [[1.0, 2.0, 3.0],
                            [4.0, 5.0, 6.0],
                            [7.0, 8.0, 9.0]]

   b <- initFloatMatrix rt [[1.0, 4.0, 7.0],
                            [2.0, 5.0, 8.0],
                            [3.0, 6.0, 9.0]]

   -- Register kernels and input data
   builtins <- loadBuiltins rt [
      ("+", floatMatrixAddBuiltin),
      ("*", floatMatrixMulBuiltin),
      ("a", dataBuiltin a),
      ("b", dataBuiltin b)]

   --let src = "(defun f (x y) (+ (* x y) (* y x))) (defun main () (f a b))"
   let src = "(defun main () ((lambda (x y) (+ (* x y) (* y x))) a b))"

   -- Evaluate an expression
--   r <- evalLisp builtins "(+ (* a b) (* b a))"
   r <- evalLispModule builtins src

   -- Display the result
   printFloatMatrix rt r
