{-# LANGUAGE LambdaCase #-}

import ViperVM.Graph.Builtins

import ViperVM.Platform.Platform
import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
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
      (w,h) = (1024, 1024) :: (Int,Int)

   putStrLn "Initializing input data"
   t <- initFloatMatrixF rt (\x y -> if x <= y then fromIntegral x+1.0 else 0.0) w h

   t11 <- allocateLinked rt (MatrixSplit 4 8) (MatrixSplitIdx 1 1) t

   let file = "apps/samples/lisp/Sample.lisp"
   ctx <- readFile =<< getDataFileName file

   myBuiltins <- loadBuiltins rt [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("mul", floatMatrixMulBuiltin),
         ("transpose", floatMatrixTransposeBuiltin),
         ("potrf", floatMatrixPotrfBuiltin),
         ("t", dataBuiltin t),
         ("t11", dataBuiltin t11)
      ]

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   r <- evalLispWithContext builtins ctx expr

   putStrLn "================\nResult:"
   printFloatMatrix rt r

   void $ releaseMany rt [t,r]

   putStrLn "Done."
