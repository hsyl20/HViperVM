{-# LANGUAGE LambdaCase #-}

import ViperVM.Graph.Builtins

import ViperVM.Platform.Platform
import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
import ViperVM.Graph.Graph
import ViperVM.Platform.Logger

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Library.FloatMatrixMul
import ViperVM.Library.FloatMatrixTranspose
import ViperVM.Library.FloatMatrixPotrf

import ViperVM.UserInterface

import ViperVM.Scheduling.Eager

import Control.Monad
import Control.Applicative ( (<$>) )
import Data.Map as Map
import System.Environment

import Paths_ViperVM

main :: IO ()
main = do

   -- Parsing command line
   expr <- getArgs >>= \case
      ["-e",s] -> return s
      [] -> return "(List.index 1 (List.index 1 (split 4 4 t)))"
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
      (w,h) = (128, 256) :: (Int,Int)

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
         ("t11", dataBuiltin t11),
         ("split", splitBuiltin rt),
         ("unsplit", unsplitBuiltin rt)
      ]

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   r <- evalLispWithContext builtins ctx expr

   putStrLn "================\nResult:"
   printFloatMatrix rt r

   void $ releaseMany rt [t,r]

   putStrLn "Done."


splitBuiltin :: Runtime -> MakeBuiltin
splitBuiltin rt rdData writeData _ _ = do

   return $ Builtin [True,True,True] $ \case
      ([ConstInteger w, ConstInteger h, d],_) -> do
         let m = rdData d
         let f = MatrixSplit (fromIntegral w) (fromIntegral h)
         let splt x y = do
               m' <- allocateLinked rt f (MatrixSplitIdx x y) m
               newNodeIO (writeData m')

         List <$> (forM [0..h-1] $ \y ->
                  newNodeIO =<< (List <$> (forM [0..w-1] $ \x ->
                     splt (fromIntegral x) (fromIntegral y))))
      _ -> error "Invalid split parameters"

unsplitBuiltin :: Runtime -> MakeBuiltin
unsplitBuiltin rt rdData writeData _ _ = do

   return $ Builtin [True] $ \case
      ([List ys],_) -> do
         -- FIXME: We suppose xs has been computed
         vs <- forM ys $ \xs -> do
                  List xs' <- getNodeExprIO xs
                  forM xs' $ \x ->
                     rdData <$> getNodeExprIO x
         d' <- unsplit rt vs
         return (writeData d')
      _ -> error "Invalid split parameters"
