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

data Kernel = CLKernel KernelName [KernelConstraint] Options KernelSource 

data CompiledKernel = CLCompiledKernel Kernel CL.CLKernel

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Processor -> IO Bool
supportConstraints cs p = liftM and $ mapM (flip supportConstraint p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Processor -> IO Bool
supportConstraint DoublePrecisionSupportRequired (CLProcessor lib _ dev) =
  liftM (not . null) $ clGetDeviceDoubleFPConfig lib dev

isOpenCLProcessor :: Processor -> Bool
isOpenCLProcessor (CLProcessor {}) = True
isOpenCLProcessor _ = False

-- | Try to compile kernel for the given processors
compileKernels :: Kernel -> [Processor] -> IO [Maybe CompiledKernel]
compileKernels ker@(CLKernel name constraints options src) procs = do
  -- Exclude non-OpenCL processors
  let clProcs = filter isOpenCLProcessor procs
  -- Exclude devices that do not support constraints
  validProcs <- filterM (supportConstraints constraints) clProcs
  -- Group devices that are in the same context to compile in one pass
  let groups = groupBy eqProc $ sortBy compareProc validProcs
  let devGroups = fmap (\x -> (extractLibCtx $ head x, fmap extractDev x)) $ groups
  programs <- traverse createProgram devGroups
  status <- liftM concat $ traverse (buildProgram options) $ zip devGroups programs
  kernels <- liftM concat $ traverse createKernel $ zip devGroups programs
  let r = zipWith (\(x,a) (_,b) -> (x,(a,b))) status kernels
  return $ fmap (returnIfValid r . extractDev) procs
  
  where
    procKey (CLProcessor lib ctx _) = (lib,ctx)
    compareProc a b = compare (procKey a) (procKey b)
    eqProc a b = (procKey a) == (procKey b)
    extractDev (CLProcessor _ _ dev) = dev
    extractLibCtx (CLProcessor lib ctx _) = (lib,ctx)
    createProgram ((lib,ctx),_) = clCreateProgramWithSource lib ctx src
    buildProgram opts (((lib,_),devs),prog) = do
      clBuildProgram lib prog devs opts
      status <- traverse (clGetProgramBuildStatus lib prog) devs :: IO [CLBuildStatus]
      return $ zip devs status
    createKernel (((lib,_),devs),prog) = do
      k <- clCreateKernel lib prog name
      return $ fmap (\x -> (x,k)) devs
    returnIfValid r dev = case lookup dev r of
      Just (CL_BUILD_SUCCESS,k) -> Just (CLCompiledKernel ker k)
      _ -> Nothing
