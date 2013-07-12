{-# LANGUAGE RecordWildCards, LambdaCase #-}
module ViperVM.Backends.OpenCL.Kernel (
   Kernel(..), KernelConfiguration(..), CompiledKernel,
   initKernel, initKernelFromFile,
   kernelCompile, kernelExecute,
   clMemParam, clIntParam, clUIntParam, clULongParam
) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Buffer
import ViperVM.Backends.OpenCL.Event
import ViperVM.Backends.OpenCL.Processor
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.KernelExecutionResult
import ViperVM.Platform.KernelParameter

import Control.Concurrent
import Control.Arrow
import Data.List (sortBy, groupBy )
import Data.Traversable (forM)
import Data.Word
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (forM_,void)

data Kernel = Kernel {
   kernelName :: String,
   kernelConstraints :: [KernelConstraint],
   kernelOptions :: String,
   kernelSource :: String,
   kernelConfigure :: [KernelParameter] -> KernelConfiguration,
   kernelMutex :: MVar ()
}

instance Eq Kernel where
  (==) k1 k2 = kernelSource k1 == kernelSource k2

instance Ord Kernel where
  compare k1 k2 = compare (kernelSource k1) (kernelSource k2)


instance Show Kernel where
   show k = "OpenCL \"" ++ kernelName k ++ "\" kernel"

data KernelConfiguration = KernelConfiguration {
   globalDim :: [Word],
   localDim :: [Word],
   parameters :: [CLKernelParameter]
} deriving (Show)

data CLKernelParameter = 
     CLMemParam CLMem
   | CLIntParam CLint
   | CLUIntParam CLuint
   | CLULongParam CLulong
   | CLBoolParam CLbool
     deriving (Show)

data CompiledKernel = CompiledKernel {
   sourceKernel :: Kernel,
   openclKernel :: CLKernel
}

initKernelFromFile :: FilePath -> String -> [KernelConstraint] -> String -> ([KernelParameter] -> KernelConfiguration) -> IO Kernel
initKernelFromFile path name consts opts conf = do
   src <- readFile path
   initKernel src name consts opts conf

initKernel :: String -> String -> [KernelConstraint] -> String -> ([KernelParameter] -> KernelConfiguration) -> IO Kernel
initKernel src name consts opts conf = do
   mtex <- newEmptyMVar
   return $ Kernel {
      kernelName = name,
      kernelConstraints = consts,
      kernelOptions = opts,
      kernelConfigure = conf,
      kernelSource = src,
      kernelMutex = mtex
   }

clMemParam :: Buffer -> CLKernelParameter
clMemParam b = CLMemParam (bufferPeer b)

clIntParam :: Int -> CLKernelParameter
clIntParam i = CLIntParam (fromIntegral i)

clUIntParam :: Word -> CLKernelParameter
clUIntParam i = CLUIntParam (fromIntegral i)

clULongParam :: Word64 -> CLKernelParameter
clULongParam i = CLULongParam (fromIntegral i)

setCLKernelArg :: OpenCLLibrary -> CLKernel -> CLuint -> CLKernelParameter -> IO ()
setCLKernelArg lib kernel idx (CLMemParam mem) = clSetKernelArgSto lib kernel idx mem
setCLKernelArg lib kernel idx (CLIntParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLUIntParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLULongParam i) = clSetKernelArgSto lib kernel idx i
setCLKernelArg lib kernel idx (CLBoolParam i) = clSetKernelArgSto lib kernel idx i


-- | Try to compile kernel for the given processors
kernelCompile :: Kernel -> [Processor] -> IO (Map Processor (Either String CompiledKernel))
kernelCompile k@(Kernel {..}) procs = do

   -- Group devices that are in the same context to compile in one pass
   let groups = groupBy eqProc $ sortBy compareProc procs

   res <- forM groups $ \procs' -> do
      let 
         devs = fmap procDevice procs'
         lib = procLibrary (head procs')
         ctx = procContext (head procs')

      prog <- clCreateProgramWithSource lib ctx kernelSource
      _ <- clBuildProgram lib prog devs kernelOptions
      buildStatus <- forM devs (clGetProgramBuildStatus lib prog)
      
      res <- forM (procs' `zip` buildStatus) $ \case 
         (proc,CL_BUILD_SUCCESS) -> do
            kernel <- clCreateKernel lib prog kernelName
            return (proc, Right (CompiledKernel k kernel))
            
         (proc,CL_BUILD_ERROR) -> do
            buildLog <- clGetProgramBuildLog lib prog (procDevice proc)
            return (proc, Left buildLog)

         (_,err) -> error ("Unexpected build status: " ++ show err)

      return (Map.fromList res)
      
   return (Map.unions res)

   where
      procKey = procLibrary &&& procContext

      compareProc a b = compare (procKey a) (procKey b)
      eqProc a b = procKey a == procKey b

-- | Execute a kernel on a given processor synchronously
kernelExecute :: Processor -> CompiledKernel -> [KernelParameter] -> IO ExecutionResult
kernelExecute proc ck params = do

   let lib = procLibrary proc
       cq  = procQueue proc
       config = kernelConfigure ker params
       mutex = kernelMutex ker
       ker = sourceKernel ck
       peer = openclKernel ck

   putMVar mutex () -- OpenCL kernels are mutable (clSetKernelArg) so we use this mutex

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib peer)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq peer (globalDim config) (localDim config) deps
   
   void $ takeMVar mutex -- Do not forget to release the mutex

   void $ clWaitForEvents lib [ev]

   return ExecuteSuccess
