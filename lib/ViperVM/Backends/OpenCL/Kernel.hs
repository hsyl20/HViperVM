{-# LANGUAGE RecordWildCards, LambdaCase #-}
module ViperVM.Backends.OpenCL.Kernel (
   Kernel(..), KernelConfiguration(..),
   initKernel, kernelExecute, CLKernelParameter(..),
   createPeerKernel
) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Event
import ViperVM.Backends.OpenCL.Processor
import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Program
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Platform.KernelConstraint
import ViperVM.STM.TMap as TMap

import Control.Concurrent
import Control.Concurrent.STM
import Data.Monoid
import Control.Monad (forM_,void)

data Kernel = Kernel {
   kernelPeers :: TMap Processor CLKernel,
   kernelConfigure :: [CLKernelParameter] -> KernelConfiguration,
   kernelConstraints :: [KernelConstraint],
   kernelMutex :: MVar (),
   kernelName :: String,
   kernelProgram :: Program
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
   globalDim :: [CLuint],
   localDim :: [CLuint],
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
   peers <- atomically TMap.empty
   return $ Kernel {
      kernelPeers = peers,
      kernelConfigure = conf,
      kernelConstraints = consts,
      kernelMutex = mtex,
      kernelName = name,
      kernelProgram = prog
   }

createPeerKernel :: Kernel -> Processor -> IO ()
createPeerKernel k proc = do
   Just prog <- programForProcessor (kernelProgram k) proc
   peer <- clCreateKernel (procLibrary proc) prog (kernelName k)
   atomically $ TMap.insert proc peer (kernelPeers k)

-- | Set kernel parameter
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

   peer <- atomically (kernelPeers ker TMap.! proc)

   putMVar mutex () -- OpenCL kernels are mutable (clSetKernelArg) so we use this mutex

   forM_ ([0..] `zip` parameters config) $ uncurry (setCLKernelArg lib peer)

   let deps = []
   ev <- clEnqueueNDRangeKernel lib cq peer (globalDim config) (localDim config) deps
   
   void $ takeMVar mutex -- Do not forget to release the mutex

   void $ clWaitForEvents lib [ev]
