{-# LANGUAGE LambdaCase #-}
module ViperVM.Platform.ObjectKernel (
      KernelObjectConfig(..), ObjectKernel(..),
      executeObjectKernel, peerKernel,
      compileObjectKernel, lockModes,
      ensureCompiledFor
   ) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Kernel
import ViperVM.Platform.KernelManager
import ViperVM.Platform.Processor
import ViperVM.Platform.Object
import ViperVM.Platform.Buffer
import ViperVM.Platform.Region
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.LockMode
import ViperVM.STM.TMap as TMap

import Control.Concurrent.STM
import Data.Foldable (forM_)
import Text.Printf

data KernelObjectConfig = KernelObjectConfig [KernelParameter] [(Buffer,Region)] [(Buffer,Region)]

data ObjectKernel = ObjectKernel Kernel [LockMode] ([Object] -> KernelObjectConfig)


instance Show ObjectKernel where
   show (ObjectKernel k _ _) = show k

peerKernel :: ObjectKernel -> Kernel
peerKernel (ObjectKernel k _ _) = k

lockModes :: ObjectKernel -> [LockMode]
lockModes (ObjectKernel _ modes _) = modes

executeObjectKernel :: ObjectManager -> Processor -> ObjectKernel -> [Object] -> IO ()
executeObjectKernel om proc ok objs = do
      atomically $ forM_ objsModes (\(o,m) -> lockObjectRetry om m o)
      executeKernel km proc k roRegions rwRegions pms
      atomically $ forM_ objs (unlockObject om)
   where
      km = kernelManager om
      ObjectKernel k modes f = ok
      objsModes = objs `zip` modes
      KernelObjectConfig pms roRegions rwRegions = f objs

compileObjectKernel :: KernelManager -> ObjectKernel -> [Processor] -> IO [Processor]
compileObjectKernel km ok = compileKernel km (peerKernel ok)



ensureCompiledFor :: KernelManager -> ObjectKernel -> Processor -> IO ()
ensureCompiledFor km k proc = do
   let 
      ker = peerKernel k
      pf = getKernelManagerPlatform km

   atomically (TMap.lookup proc (compilations ker)) >>= \case

      Just CLCompilationFailure -> 
         error "Kernel cannot be executed by the specified processor"

      Nothing -> do
         customLog pf (printf "[Compiler] Compiling kernel %s for processor %s" (show k) (show proc))
         _ <- compileObjectKernel km k [proc]
         ensureCompiledFor km k proc

      Just (CLCompilationSuccess _) -> return ()
