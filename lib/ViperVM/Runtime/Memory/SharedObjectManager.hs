{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager, objectManager,
   createSharedObjectManager, allocateInstance, releaseInstance,
   allocateSharedObject, allocateLinkedSharedObject,
   allocateTransferAttach, ensureInMemory,
   getSharedObjectManagerPlatform, unsplitSharedObject
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Link
import ViperVM.Platform.Object
import ViperVM.Platform.Objects.Matrix
import ViperVM.Platform.Memory
import ViperVM.Platform.SharedObject
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.Descriptor

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Control.Monad (foldM)
import Data.Foldable(forM_)
import Data.Set as Set
import Data.Word

data SharedObjectManager = SharedObjectManager {
   objectManager :: ObjectManager,
   lastId :: TVar Word64
}

-- | Return associated platform
getSharedObjectManagerPlatform :: SharedObjectManager -> Platform
getSharedObjectManagerPlatform = getObjectManagerPlatform . objectManager

-- | Allocate a shared object
allocateSharedObject :: SharedObjectManager -> Descriptor -> STM SharedObject
allocateSharedObject som desc = do
   soId <- readTVar (lastId som)
   writeTVar (lastId som) (soId + 1)
   createSharedObject soId desc

-- | Allocate a shared object that is a sub object of the given one
allocateLinkedSharedObject :: SharedObjectManager -> SharedObject -> LinkType -> LinkIdx -> STM SharedObject
allocateLinkedSharedObject som so typ idx = do
   
   let desc = case typ of
         MatrixSplit w h -> MatrixDesc p w h
         where p = matrixDescCellType (descriptor so)
   so' <- allocateSharedObject som desc
   subLinkTo so' typ idx so
   return so'


-- | Create a shared object manager
createSharedObjectManager :: ObjectManager -> IO SharedObjectManager
createSharedObjectManager om = atomically $ do
   SharedObjectManager om <$> newTVar 0

-- | Allocate a compatible instance of the shared object, DO NOT attach it
allocateInstance :: SharedObjectManager -> SharedObject -> Memory -> IO Object
allocateInstance som so mem = allocateFromDescriptor om mem desc
   where
      om = objectManager som
      desc = descriptor so


-- | Release an instance of the shared object
releaseInstance :: SharedObjectManager -> SharedObject -> Object -> IO ()
releaseInstance som so o = do
   let om = objectManager som
   atomically (detachInstance so o)
   releaseObject om o


-- | Allocate a new instance, transfer appropriate data from another one, then
-- associate the new instance
allocateTransferAttach :: SharedObjectManager -> SharedObject -> Object -> Memory -> IO Object
allocateTransferAttach som so src dstMem = do
   let 
      om = objectManager som
      srcMem = objectMemory src
      pf = getSharedObjectManagerPlatform som
      lks = linksBetween srcMem dstMem (links pf)
      indirectTransfer = Prelude.null lks

   -- Allocate destination
   dst <- allocateInstance som so dstMem


   -- Intermediate steps
   steps <- if indirectTransfer
      then do
         -- Use host memory as intermediate
         let hostMem = head . hostMemories $ getSharedObjectManagerPlatform som
         ho <- allocateInstance som so hostMem 
         return [ho,dst]
      else return [dst]

   let 
      f s d = do
         transferObject om s d
         atomically (attachInstance so d)
         return d

   -- Perform transfer
   foldM f src steps


-- | Ensure that an instance of a shared object is in a given memory. perform a transfer if necessary
ensureInMemory :: SharedObjectManager -> Memory -> SharedObject -> IO Object
ensureInMemory som mem so = do
   -- Try to retrieve a direct instance
   Set.elems <$> atomically (instancesInMemory so mem) >>= \case
      x:_ -> return x
      [] -> do
         -- Try to retrieve a linked instance
         Set.elems <$> atomically (linkedInstancesInMemory so mem) >>= \case
            x:_ -> return x
            [] -> do

               -- Perform transfer
               Set.elems <$> atomically (allInstances so) >>= \case
                  [] -> error "Uninitialized object accessed in read-only mode"
                  src:_ -> allocateTransferAttach som so src mem
                  -- FIXME: select source policy is not clever


-- | Unsplit a list of lists of matrices
unsplitSharedObject :: SharedObjectManager -> [[SharedObject]] -> IO SharedObject
unsplitSharedObject som os = do
   -- Compute output matrix dimensions
   let dos = fmap (fmap descriptor) os
       (row1,col1) = (head dos, fmap head dos)
       w = matrixDescWidth (head row1)
       h = matrixDescHeight (head row1)
       gw = sum (fmap matrixDescWidth row1)
       gh = sum (fmap matrixDescHeight col1)
       p = matrixDescCellType (head row1)
       desc = MatrixDesc p gw gh
       om = objectManager som

   -- Allocate output shared object
   so <- atomically (allocateSharedObject som desc)

   -- Allocate instance in host memory (FIXME we could improve this)
   let hostMem = head . hostMemories $ getSharedObjectManagerPlatform som
   MatrixObject m <- allocateInstance som so hostMem
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
