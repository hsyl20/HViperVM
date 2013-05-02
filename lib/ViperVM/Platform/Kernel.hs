{-# LANGUAGE RecordWildCards #-}

module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Processor ( Processor(..) )

import Data.List (sortBy, groupBy )
import Data.Traversable (traverse)
import Control.Monad ( liftM,forM_,void )
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
  source :: KernelSource
}

data KernelConfiguration = CLKernelConfiguration {
   globalDim :: [Int],
   localDim :: [Int],
   parameters :: [CLKernelParameter]
}

data CLKernelParameter = CLKPMem CL.CLMem
                       | CLKPInt CL.CLint

setCLKernelArg :: OpenCLLibrary -> CL.CLKernel -> CL.CLuint -> CLKernelParameter -> IO ()
setCLKernelArg lib kernel idx (CLKPMem mem) = clSetKernelArgSto lib kernel idx mem
setCLKernelArg lib kernel idx (CLKPInt i) = clSetKernelArgSto lib kernel idx i

instance Eq Kernel where
  (==) k1 k2 = source k1 == source k2

instance Ord Kernel where
  compare k1 k2 = compare (source k1) (source k2)

data CompiledKernel = CLCompiledKernel {
  kernel :: Kernel,
  peer :: CL.CLKernel
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
  programs <- traverse createProgram devGroups
  status <- liftM concat $ traverse buildProgram $ zip devGroups programs
  kernels <- liftM concat $ traverse createKernel $ zip devGroups programs
  let r = zipWith (\(x,a) (_,b) -> (x,(a,b))) status kernels
  return $ fmap (returnIfValid r . extractDev) procs
  
  where
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
      status <- traverse (clGetProgramBuildStatus lib prog) devs :: IO [CLBuildStatus]
      return $ zip devs status
    createKernel (((lib,_),devs),prog) = do
      k <- clCreateKernel lib prog kernelName
      return $ fmap (\x -> (x,k)) devs
    returnIfValid r dev = case lookup dev r of
      Just (CL_BUILD_SUCCESS,k) -> Just (CLCompiledKernel ker k)
      _ -> Nothing


-- | Execute a kernel on a given processor synchronously
execute :: Processor -> CompiledKernel -> KernelConfiguration -> IO ()

execute (CLProcessor lib _ cq _) (CLCompiledKernel _ clker) (CLKernelConfiguration {..}) = do

   -- TODO: OpenCL kernels are mutable (clSetKernelArg) so we use this mutex
   -- putMVar () (mutex ker)

   forM_ ([0..] `zip` parameters) $ uncurry (setCLKernelArg lib clker)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq clker globalDim localDim deps
   void $ clWaitForEvents lib [ev]
   
   -- TODO: Do not forget to release the mutex
   --void $ takeMVar (mutex ker)

execute proc ker _ = do
   putStrLn $ "We do not know how to execute kernel " ++ show ker ++ " on " ++ show proc
   exitFailure
