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
import ViperVM.Library.FloatMatrixTrsm

import ViperVM.UserInterface

import ViperVM.Scheduling.Eager

import Control.Monad
import Control.Applicative ( (<$>), (<*>) )
import Data.Traversable(traverse)
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
      (w,h) = (48, 48) :: (Int,Int)

   putStrLn "Initializing input data"
   t <- initFloatMatrixF rt (\x y -> if x <= y then fromIntegral (y+x)+1.0 else 0.0) w h
   a <- initFloatMatrixF rt (\x y -> fromIntegral (x+y+1 :: Int)) w h
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
         ("trsm", floatMatrixTrsmBuiltin),
         ("sgemm", floatMatrixMulBuiltin),
         ("t", dataBuiltin t),
         ("a", dataBuiltin a),
         ("b", dataBuiltin b),
         ("split", splitBuiltin rt),
         ("unsplit", unsplitBuiltin rt)
      ]

   putStrLn ("Evaluating: " ++ show expr)

   let builtins = Map.union defaultBuiltins myBuiltins
   r <- evalLispWithContext builtins ctx expr

   putStrLn ("================\nResult:" ++ show r)
   printFloatMatrix rt r

   putStrLn "Done."


splitBuiltin :: Runtime -> MakeBuiltin
splitBuiltin rt rdData writeData _ _ = do

   return $ Builtin [True,True,True] $ \case
      ([ConstInteger w', ConstInteger h', d],_) -> do
         let
            m = rdData d
            filtr = MatrixSplit w h
            (gw,gh) = matrixDescDims (descriptor m)
            (w,h) = (fromIntegral w', fromIntegral h')
            hn = (gw + (w-1)) `div` w
            vn = (gh + (h-1)) `div` h
            splt x y = do
               writeData <$> allocateLinked rt filtr (MatrixSplitIdx x y) m
            makeList xs f = foldM (\ys y -> ListCons <$> (newNodeIO =<< f y) <*> newNodeIO ys) ListNil xs

         makeList (reverse [0..vn-1]) $ \y -> do
            makeList (reverse [0..hn-1]) $ \x -> do
               splt (fromIntegral x) (fromIntegral y)
      _ -> error "Invalid split parameters"

unsplitBuiltin :: Runtime -> MakeBuiltin
unsplitBuiltin rt rdData writeData _ _ = do

   return $ Builtin [True] $ \case
      ([e],[l]) -> do
         let 
            isList (ListCons _ _) = True
            isList ListNil = True
            isList _ = False

            chk (ListCons x xs) = do
               x' <- isList <$> getNodeExprIO x
               xs' <- chk =<< getNodeExprIO xs
               return (x' && xs')
            chk ListNil = return True
            chk _ = return False

            extract :: Expr -> IO [Expr]
            extract (ListCons x xs) = do
               x' <- getNodeExprIO x
               xs' <- extract =<< getNodeExprIO xs
               return (x':xs')
            extract ListNil = return []
            extract _ = error "should not happen"

         c <- chk e
         if c 
            then do
               e' <- fmap (fmap rdData) <$> (traverse extract =<< extract e)
               writeData <$> unsplit rt e'
            else do
               dseq <- newNodeIO (Symbol "deepseq")
               l' <-newNodeIO (App dseq l)
               unsplt <- newNodeIO (Symbol "unsplit")
               return (App unsplt l')
      _ -> error "Invalid split parameters"
