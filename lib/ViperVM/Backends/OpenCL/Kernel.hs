{-# LANGUAGE RecordWildCards, LambdaCase #-}
module ViperVM.Backends.OpenCL.Kernel (
   Kernel(..), KernelConfiguration(..),
   initKernel, kernelExecute, CLKernelParameter(..),
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

import Control.Concurrent
import Data.Word
import Data.Monoid
import Control.Monad (forM_,void)

data Kernel = Kernel {
   kernelName :: String,
   kernelConstraints :: [KernelConstraint],
   kernelConfigure :: [CLKernelParameter] -> KernelConfiguration,
   kernelMutex :: MVar (),
   kernelProgram :: Program,
   kernelCLPeer :: CLKernel
}

instance Eq Kernel where
   (==) k1 k2 = and [kernelName k1 == kernelName k2,
                     kernelProgram k1 == kernelProgram k2]

instance Ord Kernel where
  compare k1 k2 = compare (kernelName k1) (kernelName k2)
                  <> compare (kernelProgram k1) (kernelProgram k2)


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

initKernel :: Program -> String -> [KernelConstraint] -> ([CLKernelParameter] -> KernelConfiguration) -> IO Kernel
initKernel prog name consts conf = do
   mtex <- newEmptyMVar
   peer <- clCreateKernel (programLib prog) (programCLPeer prog) name 
   return $ Kernel {
      kernelName = name,
      kernelConstraints = consts,
      kernelConfigure = conf,
      kernelProgram = prog,
      kernelCLPeer = peer,
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


-- | Execute a kernel on a given processor synchronously
kernelExecute :: Processor -> Kernel -> [CLKernelParameter] -> IO ()
kernelExecute proc ker params = do

   let lib = procLibrary proc
       cq  = procQueue proc
       config = kernelConfigure ker params
       mutex = kernelMutex ker
       peer = kernelCLPeer ker

   putMVar mutex () -- OpenCL kernels are mutable (clSetKernelArg) so we use this mutex

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib peer)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq peer (globalDim config) (localDim config) deps
   
   void $ takeMVar mutex -- Do not forget to release the mutex

   void $ clWaitForEvents lib [ev]
