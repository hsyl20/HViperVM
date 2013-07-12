{-# LANGUAGE RecordWildCards, TupleSections #-}

{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.Kernel where

import qualified ViperVM.Backends.OpenCL.Kernel as CL
import qualified ViperVM.Backends.OpenCL.Processor as CL
import ViperVM.Platform.Processor
import ViperVM.Platform.Compilation
import ViperVM.Platform.KernelParameter
import ViperVM.Platform.KernelConstraint
import ViperVM.Platform.KernelExecutionResult
import ViperVM.Platform.ProcessorCapabilities
import ViperVM.STM.TMap (TMap)
import qualified ViperVM.STM.TMap as TMap

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Text.Printf
import Data.Set (Set)
import qualified Data.Map as Map

data PeerKernel = CLKernel CL.Kernel
                  deriving (Show)

-- | A kernel
data Kernel = Kernel {
   peerKernel :: PeerKernel,
   compilations :: TMap Processor CompilationResult
}

instance Show Kernel where
   show k = show (peerKernel k)

-- | Try to compile kernel for the given processors
compile :: Kernel -> [Processor] -> IO ()
compile kernel procs = do
   res <- case peerKernel kernel of
      CLKernel k -> do
         let g = fmap (f (CompiledKernel . CLCompiledKernel))
         g <$> CL.kernelCompile k (getOpenCLProcessors procs)

   let cs2 = Map.fromList (fmap (\p'@(CLProcessor p _) -> (p', res Map.! p)) procs)

   atomically $ do
      cs <- readTVar (compilations kernel)
      writeTVar (compilations kernel) (Map.union cs cs2)

   where
      f _ (Left s) = CompilationFailure s
      f w (Right ck) = CompilationSuccess (w ck)

getOpenCLProcessors :: [Processor] -> [CL.Processor]
getOpenCLProcessors = fmap (\(CLProcessor p _) -> p)

-- | Processors for which the kernel is compiled
kernelCompiledFor :: Kernel -> IO [Processor]
kernelCompiledFor k = atomically $ do
   Map.keys . Map.filter isCompilationSuccess <$> readTVar (compilations k)

-- | Indicate if a kernel is compiled for a given processor
kernelIsCompiledFor :: Kernel -> Processor -> IO Bool
kernelIsCompiledFor k p = atomically $ do
   TMap.lookup p (compilations k) >>= \case
      Just res -> return (isCompilationSuccess res)
      Nothing -> return False
         

kernelEnsureCompiledFor :: Kernel -> Processor -> IO ()
kernelEnsureCompiledFor k p = do

   atomically (TMap.lookup p (compilations k)) >>= \case
      Just (CompilationFailure buildLog) -> 
         error (printf "Kernel %s cannot be compiled for processor %s:\n%s" (show k) (show p) buildLog)

      Nothing -> do
         _ <- compile k [p]
         kernelEnsureCompiledFor k p

      Just (CompilationSuccess _) -> return ()

-- | Indicate if a processor supports given constraints
supportConstraints :: [KernelConstraint] -> Processor -> Bool
supportConstraints cs p = all (`supportConstraint` p) cs

-- | Indicate if a processor supports a given constraint
supportConstraint :: KernelConstraint -> Processor -> Bool
supportConstraint DoublePrecisionSupport proc = procSupports proc DoubleFloatingPoint

-- | Indicate if a processor can execute a given kernel
canExecute :: Processor -> Kernel -> Bool
canExecute p k  = undefined --supportConstraints (CL.kernelConstraints k) p

-- | Execute a kernel on a given processor synchronously
execute :: Processor -> Kernel -> [KernelParameter] -> IO ExecutionResult
execute p k params = undefined --CL.kernelExecute p k params
