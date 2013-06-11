{-# LANGUAGE TupleSections, LambdaCase #-}

import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import ViperVM.Platform.Descriptor
import ViperVM.Platform.Platform
import ViperVM.Platform.Primitive as Prim
import ViperVM.Platform.Runtime

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Library.FloatMatrixPotrf


import ViperVM.Scheduling.Single

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Traversable (traverse)
import qualified Data.List as List
import System.Environment


main :: IO ()
main = do
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so"
   }

   putStrLn "Initializing platform..."
   pf <- initPlatform config
   rt <- initRuntime pf (singleScheduler (head (processors pf)))

   let 
      (w,h) = (32, 32)
      (w',h') = (fromIntegral w, fromIntegral h)

   putStrLn "Initializing input data"
   let triangular = [ replicate n (0.0 :: Float) ++ repeat (fromIntegral n + 1.0) | n <- [0..]]
       triangular' n = fmap (take n) (take n triangular)
       triMul n = let m = List.transpose (triangular' n) in crossWith (\xs ys -> foldl1 (+) $ zipWith (*) xs ys) m m

       crossWith f ys xs = fmap (\x -> fmap (\y -> f x y) ys) xs
       desc = MatrixDesc Prim.Float w h

   a <- pokeFloatMatrix rt desc (replicate h' (replicate w' (5.0 :: Float)))
   b <- pokeFloatMatrix rt desc (replicate h' (replicate w' (2.0 :: Float)))
   c <- pokeFloatMatrix rt desc (triMul 32)

   let
      datas = registerData [("a",a),("b",b),("c",c)]

   kernels <- traverse (loadBuiltin rt) $ Map.fromList [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("potrf", floatMatrixPotrfBuiltin)
      ]

   let builtins = Map.unions [defaultBuiltins, kernels, datas]

   expr <- getArgs >>= \case
               ["-e",s] -> return s
               [] -> return "(let* ((h (add b b))) (sub a (add h h)))"
               _ -> error "Invalid parameters"

   d <- readData <$> check builtins Map.empty expr

   result <- peekFloatMatrix rt d

   putStrLn "================\nResult:"
   traverse_ (putStrLn . show) result

   void $ releaseMany rt [a,b,c,d]

   putStrLn "Done."


registerData :: Typeable a => [(String,a)] -> Map String Builtin
registerData ds = fmap f (Map.fromList ds)
   where
      f = Builtin [] . const . return . Data . toDyn

check :: Map String Builtin -> Map String Node -> String -> IO Expr
check builtins ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show expr)
      f <- run builtins ctx r
      putStrLn ("Reduction result: " ++ show f)

      getNodeExprIO f
