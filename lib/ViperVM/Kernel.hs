module ViperVM.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Query
import ViperVM.Platform ( Processor(..) )

import Data.List (sortBy, groupBy )
import Data.Traversable (traverse)
import Control.Monad ( liftM,filterM )

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

instance Eq Kernel where
  (==) k1 k2 = source k1 == source k2

instance Ord Kernel where
  compare k1 k2 = compare (source k1) (source k2)

data CompiledKernel = CLCompiledKernel Kernel CL.CLKernel

instance Show Kernel where
  show (CLKernel name _ _ _) = "OpenCL \"" ++ name ++ "\" kernel"

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Processor -> IO Bool
supportConstraints cs p = liftM and $ mapM (`supportConstraint` p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Processor -> IO Bool
supportConstraint DoublePrecisionSupportRequired (CLProcessor lib _ dev) =
  liftM (not . null) $ clGetDeviceDoubleFPConfig lib dev
supportConstraint DoublePrecisionSupportRequired HostProcessor = return True

-- | Indicate if a processor can execute a given kernel
canExecute :: Processor -> Kernel -> IO Bool
canExecute p@(CLProcessor {}) (CLKernel _ cs _ _)  = supportConstraints cs p
canExecute _ _ = return False

isOpenCLProcessor :: Processor -> Bool
isOpenCLProcessor (CLProcessor {}) = True
isOpenCLProcessor _ = False

-- | Try to compile kernel for the given processors
compileKernels :: Kernel -> [Processor] -> IO [Maybe CompiledKernel]
compileKernels ker@(CLKernel name consts opts src) procs = do
  -- Exclude non-OpenCL processors
  let clProcs = filter isOpenCLProcessor procs
  -- Exclude devices that do not support constraints
  validProcs <- filterM (supportConstraints consts) clProcs
  -- Group devices that are in the same context to compile in one pass
  let groups = groupBy eqProc $ sortBy compareProc validProcs
  let devGroups = fmap (\x -> (extractLibCtx $ head x, fmap extractDev x)) groups
  programs <- traverse createProgram devGroups
  status <- liftM concat $ traverse buildProgram $ zip devGroups programs
  kernels <- liftM concat $ traverse createKernel $ zip devGroups programs
  let r = zipWith (\(x,a) (_,b) -> (x,(a,b))) status kernels
  return $ fmap (returnIfValid r . extractDev) procs
  
  where
    procKey (CLProcessor lib ctx _) = (lib,ctx)
    procKey _ = undefined

    compareProc a b = compare (procKey a) (procKey b)
    eqProc a b = procKey a == procKey b

    extractDev (CLProcessor _ _ dev) = dev
    extractDev _ = undefined 

    extractLibCtx (CLProcessor lib ctx _) = (lib,ctx)
    extractLibCtx _ = undefined

    createProgram ((lib,ctx),_) = clCreateProgramWithSource lib ctx src
    buildProgram (((lib,_),devs),prog) = do
      clBuildProgram lib prog devs opts
      status <- traverse (clGetProgramBuildStatus lib prog) devs :: IO [CLBuildStatus]
      return $ zip devs status
    createKernel (((lib,_),devs),prog) = do
      k <- clCreateKernel lib prog name
      return $ fmap (\x -> (x,k)) devs
    returnIfValid r dev = case lookup dev r of
      Just (CL_BUILD_SUCCESS,k) -> Just (CLCompiledKernel ker k)
      _ -> Nothing
