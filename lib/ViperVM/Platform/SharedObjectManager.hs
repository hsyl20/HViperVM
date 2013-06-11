{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager, objectManager,
   createSharedObjectManager, allocateInstance,
   allocateTransferAttach, ensureInMemory
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.Platform.SharedObject
import ViperVM.Platform.ObjectManager

import Control.Concurrent.STM
import Control.Applicative ( (<$>) )
import Data.Set as Set

data SharedObjectManager = SharedObjectManager {
   objectManager :: ObjectManager
}

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
allocateTransferAttach som so src mem = do
   let om = objectManager som

   -- Allocate
   dst <- allocateInstance som so mem

   -- Perform transfer
   transferObject om src dst

   -- Attach 
   atomically (attachObject so dst)

   return dst


-- | Ensure that an instance of a shared object is in a given memory. perform a transfer if necessary
ensureInMemory :: SharedObjectManager -> Memory -> SharedObject -> IO Object
ensureInMemory som mem so = do
   Set.elems <$> atomically (memoryObjects mem so) >>= \case
      x:_ -> return x
      [] -> Set.elems <$> atomically (objects so) >>= \case
         [] -> error "Uninitialized object accessed in read-only mode"
         src:_ -> allocateTransferAttach som so src mem
         -- FIXME: select source policy is not clever
