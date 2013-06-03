{-# LANGUAGE TupleSections, LambdaCase #-}

import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager (createBufferManager)
import ViperVM.Platform.RegionTransferManager

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Library.FloatMatrixSub
import ViperVM.Platform.Primitive as Prim

import Text.Printf
import Control.Monad
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Data.Map as Map
import Data.Dynamic
import Foreign.Marshal.Array
import Foreign.Ptr
import System.Environment


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
   tm <- createRegionTransferManager rm

   let 
      --(w,h) = (1024, 512)
      (w,h) = (32, 32)
      padding = 0
      primSize = Prim.sizeOf Prim.Float
      bufferSize = (w + padding) * h * primSize 
      openclProcs = Prelude.filter isOpenCLProcessor (processors platform)
      reg = Region2D 0 h (w*primSize) padding

   putStrLn "OpenCL processors:"
   forM_ openclProcs (putStrLn . show)

   let proc = head openclProcs
   putStrLn ("Using " ++ show proc)

   putStrLn "Registering and compiling kernels..." 
   kernelMap <- traverse id $ Map.fromList [
         ("add", floatMatrixAddKernelCL),
         ("sub", floatMatrixSubKernelCL)
      ]

   traverse_ (registerKernel km) kernelMap
   traverse_ (flip (compileKernel km) [proc]) kernelMap


   let mem = head (processorMemories proc)
       lks = links platform
       linkWrite = head (linksBetween HostMemory mem lks)
       linkRead = head (linksBetween mem HostMemory lks)

   Just a <- allocateBuffer rm mem bufferSize
   Just b <- allocateBuffer rm mem bufferSize
   Just ha <- allocateBuffer rm HostMemory bufferSize
   Just hb <- allocateBuffer rm HostMemory bufferSize
   Just hc <- allocateBuffer rm HostMemory bufferSize

   putStrLn "Initializing input data"
   let pa = getHostBufferPtr ha
       pb = getHostBufferPtr hb
       pc = getHostBufferPtr hc

   pokeArray (castPtr pa) (replicate (fromIntegral $ w*h) (5.0 :: Float))
   pokeArray (castPtr pb) (replicate (fromIntegral $ w*h) (2.0 :: Float))

   myTransfer tm linkWrite reg ha a
   myTransfer tm linkWrite reg hb b

   let kernels = Map.fromList [

         ("add", Builtin [True,True] $ \case
            (args@[x', y'],_) -> do
               let (x,y) = (readData x', readData y')
               putStrLn (printf "Executing add kernel with args %s" (show args))
               Just c <- allocateBuffer rm mem bufferSize
               let roRegions = [(x,reg),(y,reg)] 
                   rwRegions = [(c,reg)] 
                   params = [WordParam $ fromIntegral w, WordParam $ fromIntegral h, BufferParam x, BufferParam y, BufferParam c]

               executeKernel km proc (kernelMap Map.! "add") roRegions rwRegions params

               return (Data $ toDyn c)
            _ -> error "Bad kernel arguments"),

         ("sub", Builtin [True,True] $ \case
            (args@[x', y'],_) -> do
               let (x,y) = (readData x', readData y')
               putStrLn (printf "Executing sub kernel with args %s" (show args))
               Just c <- allocateBuffer rm mem bufferSize
               let roRegions = [(x,reg),(y,reg)] 
                   rwRegions = [(c,reg)] 
                   params = [WordParam $ fromIntegral w, WordParam $ fromIntegral h, BufferParam x, BufferParam y, BufferParam c]

               executeKernel km proc (kernelMap Map.! "sub") roRegions rwRegions params

               return (Data $ toDyn c)
            _ -> error "Bad kernel arguments")
         ]

       datas = registerData [("a",a),("b",b)]

   let builtins = Map.unions [defaultBuiltins, kernels, datas]

   expr <- getArgs >>= \case
               ["-e",s] -> return s
               [] -> return "(let* ((c (add b b))) (add a (add c c)))"
               _ -> error "Invalid parameters"

   c <- readData <$> check builtins Map.empty expr

   myTransfer tm linkRead reg c hc
   result <- chunks (fromIntegral w) <$> peekArray (fromIntegral $ w*h) (castPtr pc) :: IO [[Float]]

   putStrLn "================\nResult:"
   traverse_ (putStrLn . show) result

   void $ releaseBuffer rm a
   void $ releaseBuffer rm b
   void $ releaseBuffer rm c
   void $ releaseBuffer rm ha
   void $ releaseBuffer rm hb

   putStrLn "Done."

readData :: Expr -> Buffer
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


myTransfer :: RegionTransferManager -> Link -> Region -> Buffer -> Buffer -> IO ()
myTransfer tm link reg src dst = do
   putStr $ "Preparing transfer on " ++ (show link) ++ "... "
   let tr = RegionTransfer src reg [RegionTransferStep link dst reg]

   res1 <- prepareRegionTransferIO tm tr
   if res1 /= PrepareSuccess 
      then putStrLn ("ERROR: " ++ show res1)
      else putStrLn "SUCCEEDED"

   putStr " - Performing transfer... "
   res2 <- performRegionTransfer tm tr
   if any (/= RegionTransferSuccess) res2
      then putStrLn ("ERROR: " ++ show res2)
      else putStrLn "SUCCEEDED"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = (take n xs) : (chunks n (drop n xs))
