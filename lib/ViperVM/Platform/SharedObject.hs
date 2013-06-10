{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.Platform.SharedObject (
   Descriptor(..), SharedObject, descriptor, objects,
   createSharedObject, attachObject, detachObject,
   exchangeObject, objectMemories, memoryObjects
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.Platform.Descriptor
import ViperVM.STM.TSet as TSet

import Data.Typeable
import Data.Set as Set
import Control.Concurrent.STM
import Control.Applicative

-- | Shared object
-- Set of identical objects stored in different memories
data SharedObject = SharedObject Descriptor (TSet Object)
                    deriving (Typeable)

instance Show SharedObject where
   show so = "ShObj(" ++ show (descriptor so) ++ ")"


-- | Return shared-object descriptor
descriptor :: SharedObject -> Descriptor
descriptor (SharedObject desc _) = desc

-- | Return objects associated to this shared-object
objects :: SharedObject -> STM (Set Object)
objects (SharedObject _ objs) = readTVar objs

-- | Create a shared object
createSharedObject :: Descriptor -> STM SharedObject
createSharedObject desc = SharedObject desc <$> TSet.empty

-- | Attach an object to a shared object 
-- Attached object must be compatible with shared object descriptor
attachObject :: SharedObject -> Object -> STM ()
attachObject so o = do
   let SharedObject desc objs = so

       chk = case (desc,o) of
      
            (VectorDesc p sz, VectorObject v) 
               | vectorCellType v == p && vectorSize v == sz -> True
            
            (MatrixDesc p w h, MatrixObject m)
               | matrixCellType m == p && matrixWidth m == w && matrixHeight m == h -> True

            _ -> False

   if chk 
      then TSet.insert o objs
      else error "Fail"


-- | Detach an object from a shared object
detachObject :: SharedObject -> Object -> STM ()
detachObject so o = do
   let SharedObject _ objs = so
   TSet.delete o objs

-- | Exchange an object instance between two shared objects
exchangeObject :: Object -> SharedObject -> SharedObject -> STM ()
exchangeObject o src dst = do
   detachObject src o
   attachObject dst o

-- | Retrieve memories into which an instance of the object exists
objectMemories :: SharedObject -> STM (Set Memory)
objectMemories so = Set.map objectMemory <$> objects so

-- | Retrieve object instances in the given memory
memoryObjects :: Memory -> SharedObject -> STM (Set Object)
memoryObjects mem so = Set.filter ((==) mem . objectMemory) <$> objects so


