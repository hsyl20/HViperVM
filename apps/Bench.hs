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
import System.Environment

import Paths_ViperVM

main :: IO ()
main = do

   let expr = "(List.reduce add '(a a a a a a a a a a a a))"

   -- Parsing command line
   dim <- getArgs >>= \case
      ["-n",s] -> return (read s)
      [] -> return 1024
      _ -> error "Invalid parameters"

   -- Configuraing platform and runtime
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so",
      logger = (stdOutLogger <=< clocked) . filterLevel LogDebug
   }

   putStrLn "Initializing platform and runtime..."
   pf <- initPlatform config
   rt <- initRuntime pf eagerScheduler

   putStrLn "Initializing input data"
   a <- initDummyFloatMatrix rt dim dim

   let file = "apps/samples/lisp/Sample.lisp"
   ctx <- readFile =<< getDataFileName file

   myBuiltins <- loadBuiltins rt [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("mul", floatMatrixMulBuiltin),
         ("transpose", floatMatrixTransposeBuiltin),
         ("potrf", floatMatrixPotrfBuiltin),
         ("a", dataBuiltin a)
      ]

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   _ <- evalLispWithContext builtins ctx expr

   void $ releaseMany rt [a]

   putStrLn "Done."
