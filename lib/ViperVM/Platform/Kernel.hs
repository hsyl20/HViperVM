{-# LANGUAGE RecordWildCards, TupleSections #-}

{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Processor ( Processor(..), procCLDevice, procCLLib, procCLContext )
import ViperVM.Platform.Buffer
import qualified ViperVM.STM.TMap as TMap
import ViperVM.STM.TMap (TMap)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Arrow
import Control.Applicative ( (<$>) )
import Data.List (sortBy, groupBy )
import Data.Traversable (forM)
import Data.Word
import Control.Monad (forM_,void)
import System.IO.Unsafe
import qualified Data.Map as Map

type KernelName = String
type KernelSource = String
type Options = String
data KernelConstraint = DoublePrecisionSupportRequired

data CLCompilationResult = CLCompilationSuccess CL.CLKernel
                         | CLCompilationFailure String

data ExecutionResult = ExecuteSuccess 
                     | ExecuteError
                       deriving (Eq)


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

-- | Processors for which the kernel is compiled
kernelCompiledProcessors :: Kernel -> IO [Processor]
kernelCompiledProcessors k@(CLKernel {}) = do
   Map.keys . Map.filter f <$> atomically (readTVar (compilations k))
   where
      f = \case
         CLCompilationSuccess _ -> True
         _ -> False


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
} deriving (Show)

data CLKernelParameter = CLMemParam CL.CLMem
                       | CLIntParam CL.CLint
                       | CLUIntParam CL.CLuint
                       | CLULongParam CL.CLulong
                       | CLBoolParam CL.CLbool
                       deriving (Show)

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
supportConstraint DoublePrecisionSupportRequired (CLProcessor lib _ _ dev _) = not . null . unsafePerformIO $ clGetDeviceDoubleFPConfig lib dev
supportConstraint DoublePrecisionSupportRequired HostProcessor = True

-- | Indicate if a processor can execute a given kernel
canExecute :: Processor -> Kernel -> Bool
canExecute p@(CLProcessor {}) (CLKernel {..})  = supportConstraints constraints p
canExecute _ _ = False

isOpenCLProcessor :: Processor -> Bool
isOpenCLProcessor (CLProcessor {}) = True
isOpenCLProcessor _ = False

-- | Try to compile kernel for the given processors
compile :: Kernel -> [Processor] -> IO ()
compile ker@(CLKernel {..}) procs = do

   -- Exclude devices that do not support constraints
   let validProcs = filter (`canExecute` ker) procs

   -- Group devices that are in the same context to compile in one pass
   let groups = groupBy eqProc $ sortBy compareProc validProcs

   forM_ groups $ \procs' -> do
      let 
         devs = fmap procCLDevice procs'
         lib = procCLLib (head procs')
         ctx = procCLContext (head procs')

      prog <- clCreateProgramWithSource lib ctx source
      _ <- clBuildProgram lib prog devs options
      buildStatus <- forM devs (clGetProgramBuildStatus lib prog)
      
      forM_ (procs' `zip` buildStatus) $ \case 
         (proc,CL_BUILD_SUCCESS) -> do
            kernel <- clCreateKernel lib prog kernelName
            atomically (TMap.insert_ compilations proc (CLCompilationSuccess kernel))
            
         (proc,CL_BUILD_ERROR) -> do
            buildLog <- clGetProgramBuildLog lib prog (procCLDevice proc)
            atomically (TMap.insert_ compilations proc (CLCompilationFailure buildLog))

         (_,err) -> error ("Unexpected build status: " ++ show err)
      

   where
      procKey = procCLLib &&& procCLContext

      compareProc a b = compare (procKey a) (procKey b)
      eqProc a b = procKey a == procKey b



-- | Execute a kernel on a given processor synchronously
execute :: Processor -> Kernel -> [KernelParameter] -> IO ExecutionResult

execute proc@(CLProcessor lib _ cq _ _) ker@(CLKernel {}) params = do

   CLCompilationSuccess peer <- atomically (compilations ker TMap.! proc)
   let config = configure ker params

   putMVar (mutex ker) () -- OpenCL kernels are mutable (clSetKernelArg) so we use this mutex

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib peer)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq peer (globalDim config) (localDim config) deps
   
   void $ takeMVar (mutex ker) -- Do not forget to release the mutex

   void $ clWaitForEvents lib [ev]

   return ExecuteSuccess

execute _ _ _ = return ExecuteError
