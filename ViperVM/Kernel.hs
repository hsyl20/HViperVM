module ViperVM.Kernel where

import qualified ViperVM.Backends.OpenCL.Types as CL
import ViperVM.Backends.OpenCL.Program
import ViperVM.Platform ( Processor(..) )

import Data.List (sortBy, groupBy )
import Data.Traversable
import Control.Monad ( liftM )

type KernelName = String
type KernelSource = String
type Options = String
data KernelConstraint = DoubleRequired

data Kernel = CLKernel KernelName [KernelConstraint] Options KernelSource 

data CompiledKernel = CLCompiledKernel Kernel CL.CLKernel

compileKernels :: Kernel -> [Processor] -> IO [Maybe CompiledKernel]
compileKernels ker@(CLKernel name constraints options src) procs = do
  -- Group devices that are in the same context to compile in one pass
  let groups = groupBy eqProc $ sortBy compareProc procs
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
