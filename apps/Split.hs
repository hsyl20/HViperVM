{-# LANGUAGE LambdaCase #-}

import ViperVM.Graph.Builtins

import ViperVM.Platform.Platform
import ViperVM.Platform.Runtime
import ViperVM.Platform.SharedObject
import ViperVM.Platform.Descriptor
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
import Control.Monad.Loops
import Control.Applicative ( (<$>) )
import Data.Map as Map
import System.Environment

import Paths_ViperVM

main :: IO ()
main = do

   -- Parsing command line
   expr <- getArgs >>= \case
      ["-e",s] -> return s
      [] -> return "(unsplit (matmul (split 32 32 a) (split 32 32 b)))"
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
      (w,h) = (128, 128) :: (Int,Int)

   putStrLn "Initializing input data"
   t <- initFloatMatrixF rt (\x y -> if x <= y then fromIntegral x+1.0 else 0.0) w h
   a <- initFloatMatrixF rt (\x y -> fromIntegral (x+y :: Int)) w h
   let
      const2 :: Int -> Int -> Float
      const2 x y = if x == y then 2.0 else 0.0
   b <- initFloatMatrixF rt const2 w h

   let file = "apps/samples/lisp/Sample.lisp"
   ctx <- readFile =<< getDataFileName file

   myBuiltins <- loadBuiltins rt [
         ("add", floatMatrixAddBuiltin),
         ("sub", floatMatrixSubBuiltin),
         ("mul", floatMatrixMulBuiltin),
         ("transpose", floatMatrixTransposeBuiltin),
         ("potrf", floatMatrixPotrfBuiltin),
         ("t", dataBuiltin t),
         ("a", dataBuiltin a),
         ("b", dataBuiltin b),
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
      ([ConstInteger w', ConstInteger h', d],_) -> do
         let
            m = rdData d
            f = MatrixSplit w h
            (gw,gh) = matrixDescDims (descriptor m)
            (w,h) = (fromIntegral w', fromIntegral h')
            hn = (gw + (w-1)) `div` w
            vn = (gh + (h-1)) `div` h
            splt x y = do
               m' <- allocateLinked rt f (MatrixSplitIdx x y) m
               newNodeIO (writeData m')

         List <$> (forM [0..vn-1] $ \y ->
                  newNodeIO =<< (List <$> (forM [0..hn-1] $ \x ->
                     splt (fromIntegral x) (fromIntegral y))))
      _ -> error "Invalid split parameters"

unsplitBuiltin :: Runtime -> MakeBuiltin
unsplitBuiltin rt rdData writeData _ _ = do

   return $ Builtin [True] $ \case
      ([List ys],[l]) -> do
         let 
            isList (List _) = True
            isList _ = False
         chk <- allM (\x -> isList <$> getNodeExprIO x) ys
         if chk 
            then do
               vs <- forM ys $ \xs -> do
                        List xs' <- getNodeExprIO xs
                        forM xs' $ \x ->
                           rdData <$> getNodeExprIO x
               d' <- unsplit rt vs
               return (writeData d')
            else do
               dseq <- newNodeIO (Symbol "deepSeq")
               l' <-newNodeIO (App dseq l)
               unsplt <- newNodeIO (Symbol "unsplit")
               return (App unsplt l')
      _ -> error "Invalid split parameters"
