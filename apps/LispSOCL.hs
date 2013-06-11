{-# LANGUAGE LambdaCase #-}

import ViperVM.Graph.Builtins

import ViperVM.Platform.Descriptor
import ViperVM.Platform.Platform
import ViperVM.Platform.Primitive as Prim
import ViperVM.Platform.Runtime

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Library.FloatMatrixPotrf

import ViperVM.UserInterface

import ViperVM.Scheduling.Single

import Control.Monad
import Data.Foldable (traverse_)
import Data.Map as Map
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

   myBuiltins <- loadBuiltins rt [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("potrf", floatMatrixPotrfBuiltin),
         ("a", dataBuiltin a),
         ("b", dataBuiltin b),
         ("c", dataBuiltin c)
      ]

   expr <- getArgs >>= \case
               ["-e",s] -> return s
               [] -> return "(let* ((h (add b b))) (sub a (add h h)))"
               _ -> error "Invalid parameters"

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   r <- evalLisp builtins expr

   result <- peekFloatMatrix rt r

   putStrLn "================\nResult:"
   traverse_ (putStrLn . show) result

   void $ releaseMany rt [a,b,c,r]

   putStrLn "Done."
