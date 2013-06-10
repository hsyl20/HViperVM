{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager,
   createSharedObjectManager, allocateInstance
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.Platform.SharedObject
import ViperVM.Platform.ObjectManager

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
