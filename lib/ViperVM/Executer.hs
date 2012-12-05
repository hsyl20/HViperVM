{-# LANGUAGE RecordWildCards #-}

module ViperVM.Executer where

import ViperVM.Platform
import ViperVM.Kernel
import ViperVM.Backends.OpenCL.CommandQueue

import System.Exit

import Data.Foldable (forM_)

execute :: Processor -> CompiledKernel -> KernelConfiguration -> IO () -> IO ()

execute (CLProcessor lib ctx cq dev) (CLCompiledKernel ker clker) (CLKernelConfiguration {..}) callback = do

   -- TODO: OpenCL kernels are mutable (clSetKernelArg) so we use this mutex
   -- putMVar () (mutex ker)

   forM_ (parameters `zip` [0..]) $ \(p,idx) -> do
      setCLKernelArg lib clker idx p

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq clker globalDim localDim deps

   return ()
   
   -- TODO: Do not forget to release the mutex
   --void $ takeMVar (mutex ker)

execute proc ker _ _ = do
   putStrLn $ "We do not know how to execute kernel " ++ show ker ++ " on " ++ show proc
   exitFailure
