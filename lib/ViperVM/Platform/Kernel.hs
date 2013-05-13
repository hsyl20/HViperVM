{-# LANGUAGE RecordWildCards, TupleSections #-}

module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Processor ( Processor(..) )
import ViperVM.Platform.Buffer

import Control.Concurrent
import Control.Applicative ( (<$>) )
import Data.List (sortBy, groupBy )
import Data.Traversable (traverse,forM)
import Data.Word
import Control.Monad (forM_,void)
import System.Exit
import System.IO.Unsafe

type KernelName = String
type KernelSource = String
type Options = String
data KernelConstraint = DoublePrecisionSupportRequired

-- | A kernel
data Kernel = CLKernel {
   kernelName :: KernelName,
   constraints :: [KernelConstraint],
   options :: Options,
   source :: KernelSource,
   configure :: [KernelParameter] -> CLKernelConfiguration
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

data CompiledKernel = CLCompiledKernel {
  kernel :: Kernel,
  peer :: CL.CLKernel,
  mutex :: MVar ()
}


instance Show Kernel where
   show (CLKernel {..}) = "OpenCL \"" ++ kernelName ++ "\" kernel"

instance Show CompiledKernel where
   show (CLCompiledKernel {..}) = show kernel 

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
compile :: Kernel -> [Processor] -> IO [Maybe CompiledKernel]
compile ker@(CLKernel {..}) procs = do

  -- Exclude devices that do not support constraints
  let validProcs = filter (`canExecute` ker) procs

  -- Group devices that are in the same context to compile in one pass
  let groups = groupBy eqProc $ sortBy compareProc validProcs
  let devGroups = fmap (\x -> (extractLibCtx $ head x, fmap extractDev x)) groups

  devGroupsProg <- forAssocM devGroups createProgram

  status <- concat <$> forM devGroupsProg buildProgram

  kernels <- concat <$> forM devGroupsProg createKernel
  let r = zipWith (\(x,a) (_,b) -> (x,(a,b))) status kernels
  
  forM procs (returnIfValid r . extractDev)
  
  where
    forAssocM xs f = zip xs <$> traverse f xs

    procKey (CLProcessor lib ctx _ _) = (lib,ctx)
    procKey _ = undefined

    compareProc a b = compare (procKey a) (procKey b)
    eqProc a b = procKey a == procKey b

    extractDev (CLProcessor _ _ _ dev) = dev
    extractDev _ = undefined 

    extractLibCtx (CLProcessor lib ctx _ _) = (lib,ctx)
    extractLibCtx _ = undefined

    createProgram ((lib,ctx),_) = clCreateProgramWithSource lib ctx source

    buildProgram (((lib,_),devs),prog) = do
      clBuildProgram lib prog devs options
      forAssocM devs (clGetProgramBuildStatus lib prog)

    createKernel (((lib,_),devs),prog) = do
      k <- clCreateKernel lib prog kernelName
      return $ fmap (,k) devs

    returnIfValid r dev = do
      case lookup dev r of
         Just (CL_BUILD_SUCCESS,k) -> do
            m <- newEmptyMVar
            return $ Just (CLCompiledKernel ker k m)
         _ -> return Nothing


-- | Execute a kernel on a given processor synchronously
execute :: Processor -> CompiledKernel -> [KernelParameter] -> IO ()

execute (CLProcessor lib _ cq _) ck@(CLCompiledKernel {}) params = do

   -- TODO: OpenCL kernels are mutable (clSetKernelArg) so we use this mutex
   putMVar (mutex ck) ()

   let config = configure (kernel ck) params

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib (peer ck))

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq (peer ck) (globalDim config) (localDim config) deps
   void $ clWaitForEvents lib [ev]
   
   -- TODO: Do not forget to release the mutex
   void $ takeMVar (mutex ck)

execute proc ker _ = do
   putStrLn $ "We do not know how to execute kernel " ++ show ker ++ " on " ++ show proc
   exitFailure
