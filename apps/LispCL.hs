{-# LANGUAGE TupleSections, LambdaCase #-}

import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager (createBufferManager)

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Platform.Primitive as Prim

import Text.Printf
import Control.Monad
import Data.Map as Map
import Data.Dynamic


main :: IO ()
main = do
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so"
   }

   putStrLn "Initializing platform..."
   platform <- initPlatform config

   bm <- createBufferManager platform
   rm <- createRegionLockManager bm
   km <- createKernelManager rm

   let (w,h) = (1024, 512)
       padding = 11
       bufferSize = (w + padding) * h * (Prim.sizeOf Prim.Float) 
       openclProcs = Prelude.filter isOpenCLProcessor (processors platform)
       ker = floatMatrixAddKernelCL
       reg = Region2D 0 h w padding

   putStrLn "Registering kernel..." 
   registerKernel km ker

   putStrLn "OpenCL processors:"
   forM_ openclProcs (putStrLn . show)

   putStrLn "\nCompiling kernel..." 
   validProcs <- compileKernel km ker openclProcs

   putStrLn "Compilation succeeded for processors:" 
   forM_ validProcs (putStrLn . show)
   putStrLn ""

   let proc = head validProcs
   putStrLn ("Using " ++ show proc)

   let mem = head (processorMemories proc)

   Just a <- allocateBuffer rm mem bufferSize
   Just b <- allocateBuffer rm mem bufferSize

   let kernels = Map.fromList [

         ("add", Builtin [True,True] $ \case
            (args@[Data x', Data y'],_) -> do
               let readData u = fromDyn u (error "Invalid data")
                   (x,y) = (readData x', readData y')
               putStrLn (printf "Executing kernel with args %s" (show args))
               Just c <- allocateBuffer rm mem bufferSize
               let roRegions = [(x,reg),(y,reg)] 
                   rwRegions = [(c,reg)] 
                   params = [WordParam $ fromIntegral w, WordParam $ fromIntegral h, BufferParam x, BufferParam y, BufferParam c]

               executeKernel km proc ker roRegions rwRegions params

               return (Data $ toDyn c)
            _ -> error "Bad kernel arguments")
         ]

       datas = registerData [("a",a),("b",b)]

   let builtins = Map.unions [defaultBuiltins, kernels, datas]
       expr = "(add a b)"

   check builtins Map.empty expr

   void $ releaseBuffer rm a
   void $ releaseBuffer rm b

   putStrLn "Done."


registerData :: Typeable a => [(String,a)] -> Map String Builtin
registerData ds = fmap f (Map.fromList ds)
   where
      f = Builtin [] . const . return . Data . toDyn

check :: Map String Builtin -> Map String Node -> String -> IO ()
check builtins ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show expr)
      f <- run builtins ctx r
      putStrLn ("Reduction result: " ++ show f)
