{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager, objectManager,
   createSharedObjectManager, allocateInstance,
   allocateTransferAttach, ensureInMemory,
   getSharedObjectManagerPlatform
) where

import ViperVM.Platform.Platform
import ViperVM.Platform.Link
import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.Platform.SharedObject
import ViperVM.Platform.ObjectManager

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Control.Monad (foldM)
import Data.Set as Set

data SharedObjectManager = SharedObjectManager {
   objectManager :: ObjectManager
}

-- | Return associated platform
getSharedObjectManagerPlatform :: SharedObjectManager -> Platform
getSharedObjectManagerPlatform = getObjectManagerPlatform . objectManager

-- | Create a shared object manager
createSharedObjectManager :: ObjectManager -> IO SharedObjectManager
createSharedObjectManager om = do
   return (SharedObjectManager om)

-- | Allocate a compatible instance of the shared object, DO NOT atach it
allocateInstance :: SharedObjectManager -> SharedObject -> Memory -> IO Object
allocateInstance som so mem = allocateFromDescriptor om mem desc
   where
      om = objectManager som
      desc = descriptor so


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
                  ho <- allocateInstance som so HostMemory -- Use host memory as intermediate
                  return [ho,dst]
               else return [dst]

   let 
      f s d = do
         transferObject om s d
         atomically (attachObject so d)
         return d

   -- Perform transfer
   foldM f src steps


-- | Ensure that an instance of a shared object is in a given memory. perform a transfer if necessary
ensureInMemory :: SharedObjectManager -> Memory -> SharedObject -> IO Object
ensureInMemory som mem so = do
   Set.elems <$> atomically (memoryObjects mem so) >>= \case

      x:_ -> return x

      [] -> Set.elems <$> atomically (objects so) >>= \case
         [] -> error "Uninitialized object accessed in read-only mode"
         src:_ -> allocateTransferAttach som so src mem
         -- FIXME: select source policy is not clever
