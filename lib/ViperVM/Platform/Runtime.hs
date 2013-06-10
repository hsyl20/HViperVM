module ViperVM.Platform.Runtime (
      initRuntime, allocate, release, releaseMany,
      initFloatMatrix,
      execute
   ) where

import ViperVM.Platform (Platform, Configuration, initPlatform)
import ViperVM.Platform.Memory
import ViperVM.Platform.KernelManager
import ViperVM.Platform.ObjectKernel
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.BufferManager
import ViperVM.Platform.RegionTransferManager
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.SharedObjectManager
import ViperVM.Platform.SharedObject

import Data.Foldable (traverse_)
import Text.Printf
import Control.Concurrent.STM

data Runtime = Runtime {
      platform :: Platform,
      bufferManager :: BufferManager,
      regionManager :: RegionLockManager,
      kernelManager :: KernelManager,
      transferManager :: RegionTransferManager,
      objectManager :: ObjectManager,
      shareObjectManager :: SharedObjectManager
   }


-- | Initialize a runtime system
initRuntime :: Configuration -> IO Runtime
initRuntime config = do
   pf <- initPlatform config

   bm <- createBufferManager pf
   rm <- createRegionLockManager bm
   km <- createKernelManager rm
   tm <- createRegionTransferManager rm
   om <- createObjectManager tm km
   som <- createSharedObjectManager om

   return $ Runtime pf bm rm km tm om som

-- | Allocate a new data
allocate :: Runtime -> Descriptor -> IO SharedObject
allocate rt desc = do
   so <- atomically (createSharedObject desc)
   return so


-- | Release data
release :: Runtime -> SharedObject -> IO ()
release rt so = return ()

-- | Release several data
releaseMany :: Runtime -> [SharedObject] -> IO ()
releaseMany rt = traverse_ (release rt)

-- | Allocate and initialize a matrix of floats
initFloatMatrix :: Runtime -> Descriptor -> [[Float]] -> IO SharedObject
initFloatMatrix rt desc ds = do
   let 
      om = objectManager rt
      (MatrixDesc prim w h) = desc

   so <- allocate rt desc
   
   Just o <- allocateMatrixObject om HostMemory prim w h 0

   pokeHostFloatMatrix om o ds

   atomically (attachObject so o)

   return so


-- | Execute the given kernel
execute :: Runtime -> ObjectKernel -> [SharedObject] -> IO ()
execute rt k args = do
   putStrLn (printf "Execute %s with params %s" (show k) (show args))
