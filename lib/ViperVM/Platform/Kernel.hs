{-# LANGUAGE RecordWildCards, TupleSections #-}

module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Processor ( Processor(..), procCLDevice )
import ViperVM.Platform.Buffer
import qualified ViperVM.STM.TMap as TMap
import ViperVM.STM.TMap (TMap)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Data.List (sortBy, groupBy )
import Data.Traversable (traverse,forM)
import Data.Word
import Data.Maybe
import Control.Monad (forM_,void)
import System.Exit
import System.IO.Unsafe

type KernelName = String
type KernelSource = String
type Options = String
data KernelConstraint = DoublePrecisionSupportRequired

data CLCompilationResult = CLCompilationSuccess CL.CLKernel
                         | CLCompilationFailure

-- | A kernel
data Kernel = CLKernel {
   kernelName :: KernelName,
   constraints :: [KernelConstraint],
   options :: Options,
   source :: KernelSource,
   configure :: [KernelParameter] -> CLKernelConfiguration,
   compilations :: TMap Processor CLCompilationResult,
   mutex :: MVar ()
}

initCLKernelFromFile :: FilePath -> String -> [KernelConstraint] -> Options -> ([KernelParameter] -> CLKernelConfiguration) -> IO Kernel
initCLKernelFromFile path name consts opts conf = do
   src <- readFile path
   initCLKernel src name consts opts conf

initCLKernel :: String -> String -> [KernelConstraint] -> Options -> ([KernelParameter] -> CLKernelConfiguration) -> IO Kernel
initCLKernel src name consts opts conf = do
   compiled <- atomically TMap.empty
   mtex <- newEmptyMVar
   return $ CLKernel {
      kernelName = name,
      constraints = consts,
      options = opts,
      configure = conf,
      source = src,
      compilations = compiled,
      mutex = mtex
   }

data KernelParameter = IntParam Int |
                       WordParam Word |
                       FloatParam Float |
                       DoubleParam Double |
                       BufferParam Buffer
                       deriving (Show)

data CLKernelConfiguration = CLKernelConfiguration {
   globalDim :: [Word],
   localDim :: [Word],
   parameters :: [CLKernelParameter]
}

data CLKernelParameter = CLMemParam CL.CLMem
                       | CLIntParam CL.CLint
                       | CLUIntParam CL.CLuint
                       | CLULongParam CL.CLulong
                       | CLBoolParam CL.CLbool

clMemParam :: Buffer -> CLKernelParameter
clMemParam b = CLMemParam (getCLBuffer b)

clIntParam :: Int -> CLKernelParameter
clIntParam i = CLIntParam (fromIntegral i)

clUIntParam :: Word -> CLKernelParameter
clUIntParam i = CLUIntParam (fromIntegral i)

clULongParam :: Word64 -> CLKernelParameter
clULongParam i = CLULongParam (fromIntegral i)

setCLKernelArg :: OpenCLLibrary -> CL.CLKernel -> CL.CLuint -> CLKernelParameter -> IO ()
setCLKernelArg lib kernel idx (CLMemParam mem) = clSetKernelArgSto lib kernel idx mem
setCLKernelArg lib kernel idx (CLIntParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLUIntParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLULongParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLBoolParam i) = clSetKernelArgSto lib kernel idx i

instance Eq Kernel where
  (==) k1 k2 = source k1 == source k2

instance Ord Kernel where
  compare k1 k2 = compare (source k1) (source k2)


instance Show Kernel where
   show (CLKernel {..}) = "OpenCL \"" ++ kernelName ++ "\" kernel"

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Processor -> Bool
supportConstraints cs p = all (`supportConstraint` p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Processor -> Bool
supportConstraint DoublePrecisionSupportRequired (CLProcessor lib _ _ dev) = not . null . unsafePerformIO $ clGetDeviceDoubleFPConfig lib dev
supportConstraint DoublePrecisionSupportRequired HostProcessor = True

-- | Indicate if a processor can execute a given kernel
canExecute :: Processor -> Kernel -> Bool
canExecute p@(CLProcessor {}) (CLKernel {..})  = supportConstraints constraints p
canExecute _ _ = False

isOpenCLProcessor :: Processor -> Bool
isOpenCLProcessor (CLProcessor {}) = True
isOpenCLProcessor _ = False

-- | Try to compile kernel for the given processors
compile :: Kernel -> [Processor] -> IO [Processor]
compile ker@(CLKernel {..}) procs = do

   -- Exclude devices that do not support constraints
   let validProcs = filter (`canExecute` ker) procs

   -- Group devices that are in the same context to compile in one pass
   let groups = groupBy eqProc $ sortBy compareProc validProcs
   let devGroups = fmap (\x -> (extractLibCtx $ head x, fmap procCLDevice x)) groups

   devGroupsProg <- forAssocM devGroups createProgram

   status <- concat <$> forM devGroupsProg buildProgram

   kernels <- concat <$> forM devGroupsProg createKernel
   let r = zipWith (\(x,a) (_,b) -> (x,(a,b))) status kernels

   ps <- forM procs $ \proc -> do
      let dev = procCLDevice proc
      case lookup dev r of
         Just (CL_BUILD_SUCCESS,k) -> atomically $ do
            TMap.insert_ compilations proc (CLCompilationSuccess k)
            return (Just proc)
         _ -> atomically $ do
            TMap.insert_ compilations proc CLCompilationFailure
            return Nothing

   return (catMaybes ps)
  
   where
      forAssocM xs f = zip xs <$> traverse f xs

      procKey (CLProcessor lib ctx _ _) = (lib,ctx)
      procKey _ = undefined

      compareProc a b = compare (procKey a) (procKey b)
      eqProc a b = procKey a == procKey b

      extractLibCtx (CLProcessor lib ctx _ _) = (lib,ctx)
      extractLibCtx _ = undefined

      createProgram ((lib,ctx),_) = clCreateProgramWithSource lib ctx source

      buildProgram (((lib,_),devs),prog) = do
        clBuildProgram lib prog devs options
        forAssocM devs (clGetProgramBuildStatus lib prog)

      createKernel (((lib,_),devs),prog) = do
        k <- clCreateKernel lib prog kernelName
        return $ fmap (,k) devs



-- | Execute a kernel on a given processor synchronously
execute :: Processor -> Kernel -> [KernelParameter] -> IO ()

execute proc@(CLProcessor lib _ cq _) ker@(CLKernel {}) params = do

   CLCompilationSuccess peer <- atomically (compilations ker TMap.! proc)
   let config = configure ker params

   putMVar (mutex ker) () -- OpenCL kernels are mutable (clSetKernelArg) so we use this mutex

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib peer)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq peer (globalDim config) (localDim config) deps
   
   void $ takeMVar (mutex ker) -- Do not forget to release the mutex

   void $ clWaitForEvents lib [ev]

execute proc ker _ = do
   putStrLn $ "We do not know how to execute kernel " ++ show ker ++ " on " ++ show proc
   exitFailure
