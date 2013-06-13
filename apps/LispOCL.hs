{-# LANGUAGE TupleSections, LambdaCase #-}

import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import ViperVM.Platform
import ViperVM.Platform.Logger
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager (createBufferManager)
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.ObjectManager

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Library.FloatMatrixPotrf

import ViperVM.Platform.Primitive as Prim

import Text.Printf
import Control.Monad
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Data.Map as Map
import qualified Data.List as List
import Data.Dynamic
import System.Environment


main :: IO ()
main = do
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so",
      logger = stdOutLogger . filterLevel LogDebug
   }

   putStrLn "Initializing platform..."
   platform <- initPlatform config

   bm <- createBufferManager platform
   rm <- createRegionLockManager bm
   km <- createKernelManager rm
   tm <- createRegionTransferManager rm
   om <- createObjectManager tm km

   let 
      --(w,h) = (1024, 512)
      (w,h) = (32, 32)
      (w',h') = (fromIntegral w, fromIntegral h)
      padding = 0
      openclProcs = Prelude.filter isOpenCLProcessor (processors platform)

   putStrLn "OpenCL processors:"
   forM_ openclProcs (putStrLn . show)

   let proc = head openclProcs
   putStrLn ("Using " ++ show proc)

   putStrLn "Registering and compiling kernels..." 
   kernelMap <- traverse id $ Map.fromList [
         ("add", floatMatrixAddObjectKernelCL),
         ("sub", floatMatrixSubObjectKernelCL),
         ("potrf", floatMatrixPotrfObjectKernelCL)
      ]

   traverse_ (flip (compileObjectKernel km) [proc]) kernelMap


   let mem = head (processorMemories proc)

   putStrLn "Initializing input data"
   let triangular = [ replicate n (0.0 :: Float) ++ repeat (fromIntegral n + 1.0) | n <- [0..]]
       triangular' n = fmap (take n) (take n triangular)
       triMul n = let m = List.transpose (triangular' n) in crossWith (\xs ys -> foldl1 (+) $ zipWith (*) xs ys) m m

       crossWith f ys xs = fmap (\x -> fmap (\y -> f x y) ys) xs

   Just ha <- allocateMatrixObject om HostMemory Prim.Float w h padding
   Just hb <- allocateMatrixObject om HostMemory Prim.Float w h padding
   Just hc <- allocateMatrixObject om HostMemory Prim.Float w h padding

   pokeHostFloatMatrix om ha (replicate h' (replicate w' (5.0 :: Float)))
   pokeHostFloatMatrix om hb (replicate h' (replicate w' (2.0 :: Float)))
   pokeHostFloatMatrix om hc (triMul 32)

   Just a <- allocateMatrixObject om mem Prim.Float w h padding
   Just b <- allocateMatrixObject om mem Prim.Float w h padding
   Just c <- allocateMatrixObject om mem Prim.Float w h padding

   transferObject om ha a
   transferObject om hb b
   transferObject om hc c

   let kernels = Map.fromList [

         ("add", Builtin [True,True] $ \case
            (args@[x', y'],_) -> do
               let (x,y) = (readData x', readData y')
               putStrLn (printf "Executing add kernel with args %s" (show args))
               Just e <- allocateMatrixObject om mem Prim.Float w h padding
               executeObjectKernel om proc (kernelMap Map.! "add") [x,y,e]
               return (Data $ toDyn e)
            _ -> error "Bad kernel arguments"),

         ("sub", Builtin [True,True] $ \case
            (args@[x', y'],_) -> do
               let (x,y) = (readData x', readData y')
               putStrLn (printf "Executing sub kernel with args %s" (show args))
               Just e <- allocateMatrixObject om mem Prim.Float w h padding
               executeObjectKernel om proc (kernelMap Map.! "sub") [x,y,e]
               return (Data $ toDyn c)
            _ -> error "Bad kernel arguments"),

         ("potrf", Builtin [True] $ \case
            (args@[x'],_) -> do
               let x = readData x'
               putStrLn (printf "Executing potrf kernel with args %s" (show args))
               Just e <- allocateMatrixObject om mem Prim.Float w h padding
               executeObjectKernel om proc (kernelMap Map.! "potrf") [x,e]
               return (Data $ toDyn e)
            _ -> error "Bad kernel arguments")
         ]

       datas = registerData [("a",a),("b",b),("c",c)]

   let builtins = Map.unions [defaultBuiltins, kernels, datas]

   expr <- getArgs >>= \case
               ["-e",s] -> return s
               [] -> return "(let* ((h (add b b))) (add a (add h h)))"
               _ -> error "Invalid parameters"

   d <- readData <$> check builtins Map.empty expr

   transferObject om d hc
   result <- peekHostFloatMatrix om hc

   putStrLn "================\nResult:"
   traverse_ (putStrLn . show) result

   void $ releaseObject om a
   void $ releaseObject om b
   void $ releaseObject om c
   void $ releaseObject om d
   void $ releaseObject om ha
   void $ releaseObject om hb
   void $ releaseObject om hc

   putStrLn "Done."

readData :: Expr -> Object
readData (Data u) = fromDyn u (error "Invalid data")
readData _ = error "Invalid data parameter"

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
