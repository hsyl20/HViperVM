{-# LANGUAGE LambdaCase #-}

module ViperVM.Runtime.Memory.VirtualMemory (
   VirtualMemory, objectManager,
   createVirtualMemory, allocateInstance, releaseInstance,
   allocateVirtualObject, allocateLinkedVirtualObject,
   allocateTransferAttach, ensureInMemory,
   virtualMemoryPlatform, unsplitVirtualObject
) where

import ViperVM.STM.TSet as TSet
import ViperVM.Platform.Platform
import ViperVM.Platform.Link
import ViperVM.Runtime.Memory.Object
import ViperVM.Runtime.Memory.Objects.Matrix
import ViperVM.Platform.Memory
import ViperVM.Runtime.Memory.VirtualObject

import Control.Concurrent.STM
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (foldM)
import Data.Foldable(forM_)
import Data.Set as Set
import Data.Word

data VirtualMemory = VirtualMemory {
   virtualMemoryPlatform :: Platform,
   lastId :: TVar Word64,
   objects :: TSet VirtualObject
}

-- | Create a shared object manager
initVirtualMemory :: Platform -> STM VirtualMemory
initVirtualMemory pf = do
   VirtualMemory pf <$> newTVar 0 <*> TSet.empty

initVirtualMemoryIO :: Platform -> IO VirtualMemory
initVirtualMemoryIO pf = atomically (initVirtualMemory pf)

-- | Allocate a virtual object
allocate :: VirtualMemory -> Descriptor -> STM VirtualObject
allocate mem desc = do
   soId <- readTVar (lastId mem)
   writeTVar (lastId mem) (soId + 1)
   obj <- createVirtualObject soId desc
   TSet.insert (objects mem) obj
   return obj

allocateIO :: VirtualMemory -> Descriptor -> IO VirtualObject
allocateIO mem desc = atomically (allocate mem desc)

-- | Allocate a shared object that is a sub object of the given one
allocateLinkedVirtualObject :: VirtualMemory -> VirtualObject -> LinkType -> LinkIdx -> STM VirtualObject
allocateLinkedVirtualObject mem so typ idx = do
   
   let desc = case typ of
         MatrixSplit w h -> MatrixDesc p w h
         where p = matrixDescCellType (descriptor so)
   so' <- allocate mem desc
   subLinkTo so' typ idx so
   return so'




-- | Unsplit a list of lists of matrices
unsplitVirtualObject :: VirtualMemory -> [[VirtualObject]] -> IO VirtualObject
unsplitVirtualObject mem os = do
   -- Compute output matrix dimensions
   let dos = fmap (fmap descriptor) os
       (row1,col1) = (head dos, fmap head dos)
       w = matrixDescWidth (head row1)
       h = matrixDescHeight (head row1)
       gw = sum (fmap matrixDescWidth row1)
       gh = sum (fmap matrixDescHeight col1)
       p = matrixDescCellType (head row1)
       desc = MatrixDesc p gw gh
       om = objectManager mem

   -- Allocate output shared object
   so <- atomically (allocate mem desc)

   -- Allocate instance in host memory (FIXME we could improve this)
   let hostMem = head . hostMemories $ virtualMemoryPlatform mem
   MatrixObject m <- allocateInstance mem so hostMem
   let ms = matrixSplit m w h

   let srcDst = zip (concat os) (concat ms)

   -- Perform transfers
   -- FIXME: transfers are performed sequentially
   forM_ srcDst $ \(src,dst) -> do
      Set.elems <$> atomically (allInstances src) >>= \case
         [] -> error "Uninitialized object accessed in read-only mode"
         srcInstance:_ -> do
            transferObject om srcInstance (MatrixObject dst)
   
   -- Attach and return instance
   atomically (attachInstance so (MatrixObject m))
   return so
