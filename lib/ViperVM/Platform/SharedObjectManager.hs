{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager,
   createSharedObjectManager, allocateInstance
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.Platform.SharedObject
import ViperVM.STM.TSet as TSet
import ViperVM.Platform.ObjectManager

import Control.Concurrent.STM
import Control.Applicative

data SharedObjectManager = SharedObjectManager {
   objectManager :: ObjectManager
}

-- | Create a shared object manager
createSharedObjectManager :: ObjectManager -> IO SharedObjectManager
createSharedObjectManager om = do
   return (SharedObjectManager om)

-- | Allocate a compatible instance of the shared object, DO NOT atach it
allocateInstance :: SharedObjectManager -> SharedObject -> Memory -> IO Object
allocateInstance som so mem = do
   let om = objectManager som

   case descriptor so of
      MatrixDesc prim w h -> do
         let padding = w `mod` 32
         allocateMatrix om mem prim w h padding >>= \case
            Nothing -> error "Unable to allocate matrix"
            Just m -> return (MatrixObject m)

      VectorDesc prim sz -> do
         allocateVector om mem prim sz >>= \case
            Nothing -> error "Unable to allocate vector"
            Just v -> return (VectorObject v)
         
