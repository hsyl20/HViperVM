{-# LANGUAGE LambdaCase #-}

import ViperVM.Graph.Builtins

import ViperVM.Platform.Platform
import ViperVM.Platform.Runtime
import ViperVM.Platform.Logger

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Library.FloatMatrixMul
import ViperVM.Library.FloatMatrixTranspose
import ViperVM.Library.FloatMatrixPotrf

import ViperVM.UserInterface

import ViperVM.Scheduling.Eager

import Control.Monad
import Data.Map as Map
import qualified Data.List as List
import System.Environment

import Paths_ViperVM

main :: IO ()
main = do

   -- Parsing command line
   expr <- getArgs >>= \case
      ["-e",s] -> return s
      [] -> return "(let* ((h (add b b))) (sub a (add h h)))"
      _ -> error "Invalid parameters"

   -- Configuraing platform and runtime
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so",
      logger = (stdOutLogger <=< clocked) . filterLevel LogDebug
   }

   putStrLn "Initializing platform and runtime..."
   pf <- initPlatform config
   rt <- initRuntime pf eagerScheduler

   let
      (w,h) = (32, 32) :: (Int,Int)
      triangular = [ replicate n (0.0 :: Float) ++ repeat (fromIntegral n + 1.0) | n <- [0..]]
      triangular' n = fmap (take n) (take n triangular)
      triMul n = let m = List.transpose (triangular' n) in crossWith (\xs ys -> foldl1 (+) $ zipWith (*) xs ys) m m

      crossWith f ys xs = fmap (\x -> fmap (\y -> f x y) ys) xs

   putStrLn "Initializing input data"
   a <- initFloatMatrixF rt (\_ _ -> 5.0) w h
   b <- initFloatMatrixF rt (\_ _ -> 2.0) w h
   c <- initFloatMatrix rt (triMul 32)
   t <- initFloatMatrixF rt (\x y -> if x <= y then fromIntegral x+1.0 else 0.0) w h

   let file = "apps/samples/lisp/Sample.lisp"
   ctx <- readFile =<< getDataFileName file

   myBuiltins <- loadBuiltins rt [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("mul", floatMatrixMulBuiltin),
         ("transpose", floatMatrixTransposeBuiltin),
         ("potrf", floatMatrixPotrfBuiltin),
         ("a", dataBuiltin a),
         ("b", dataBuiltin b),
         ("c", dataBuiltin c),
         ("t", dataBuiltin t)
      ]

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   r <- evalLispWithContext builtins ctx expr

   putStrLn "================\nResult:"
   printFloatMatrix rt r

   void $ releaseMany rt [a,b,c,t,r]

   putStrLn "Done."
