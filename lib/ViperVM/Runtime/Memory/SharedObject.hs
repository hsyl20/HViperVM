{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.Platform.SharedObject (
   Descriptor(..), SharedObject, descriptor, instances,
   createSharedObject, attachInstance, detachInstance,
   exchangeInstance, instanceMemories, instancesInMemory,
   subLinkTo, allInstances, linkedInstancesInMemory,
   LinkType(..), LinkIdx(..)
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Objects.Vector
import ViperVM.Platform.Objects.Matrix
import ViperVM.Platform.Memory
import ViperVM.Platform.Descriptor
import ViperVM.STM.TSet as TSet

import Data.Typeable
import Data.Word
import Data.Set as Set
import Control.Concurrent.STM
import Control.Applicative
import Data.Traversable (forM)

-- | Shared object
-- Set of identical objects stored in different memories
data SharedObject = SharedObject {
   soID :: Word64,
   descriptor :: Descriptor,
   objects :: TSet Object,
   parents :: TSet SubLink,
   children :: TSet SharedObject
} deriving (Typeable)

instance Eq SharedObject where
   (==) a b = soID a == soID b

instance Ord SharedObject where
   compare a b = compare (soID a) (soID b)

instance Show SharedObject where
   show so = "ShObj(" ++ show (descriptor so) ++ ")"

data LinkType = MatrixSplit Word64 Word64 deriving (Eq,Ord)
data LinkIdx = MatrixSplitIdx Int Int deriving (Eq,Ord)
data SubLink = SubLink SharedObject LinkType LinkIdx deriving (Eq,Ord)

-- | Create a shared object
createSharedObject :: Word64 -> Descriptor -> STM SharedObject
createSharedObject identifier desc = do
   SharedObject identifier desc <$> TSet.empty <*> TSet.empty <*> TSet.empty

-- | Return objects associated to this shared-object
instances :: SharedObject -> STM (Set Object)
instances = readTVar . objects

-- | Attach an object to a shared object 
-- Attached object must be compatible with shared object descriptor
attachInstance :: SharedObject -> Object -> STM ()
attachInstance so o = do

   let chk = case (descriptor so,o) of
      
         (VectorDesc p sz, VectorObject v) 
            | vectorCellType v == p && vectorSize v == sz -> True
            
         (MatrixDesc p w h, MatrixObject m)
            | matrixCellType m == p && matrixWidth m == w && matrixHeight m == h -> True

         _ -> False

   if chk 
      then TSet.insert o (objects so)
      else error "Fail"


-- | Detach an object from a shared object
detachInstance :: SharedObject -> Object -> STM ()
detachInstance so o = do
   TSet.delete o (objects so)

-- | Exchange an object instance between two shared objects
exchangeInstance :: Object -> SharedObject -> SharedObject -> STM ()
exchangeInstance o src dst = do
   detachInstance src o
   attachInstance dst o

-- | Retrieve memories into which an instance of the object exists
instanceMemories :: SharedObject -> STM (Set Memory)
instanceMemories so = Set.map objectMemory <$> instances so

-- | Retrieve object instances in the given memory
instancesInMemory :: SharedObject -> Memory -> STM (Set Object)
instancesInMemory so mem = Set.filter f <$> instances so
   where
      f = (==) mem . objectMemory 

-- | Retrieve object instances in the given memory
linkedInstancesInMemory :: SharedObject -> Memory -> STM (Set Object)
linkedInstancesInMemory so mem = Set.filter f <$> linkedInstances so
   where
      f = (==) mem . objectMemory 

-- | Attach a parent data
subLinkTo :: SharedObject -> LinkType -> LinkIdx -> SharedObject -> STM ()
subLinkTo child linkType linkIdx parent = do
   let subLink = SubLink parent linkType linkIdx
   TSet.insert subLink (parents child) 
   TSet.insert child (children parent) 

-- | Retrieve all instances of a shared object
allInstances :: SharedObject -> STM (Set Object)
allInstances so = Set.union <$> instances so <*> linkedInstances so

-- | Retrieve instances from linked objects
linkedInstances :: SharedObject -> STM (Set Object)
linkedInstances so = do
   links <- readTVar (parents so)

   os <- forM (Set.elems links) $ \(SubLink so' typ idx) -> do
      -- Recursively retrieve all instances
      is <- allInstances so'

      -- Retrieve filter
      let f = case (typ,idx) of 
               (MatrixSplit w h, MatrixSplitIdx x y) -> 
                  (\(MatrixObject o) -> MatrixObject (matrixSplit o w h !! y !! x))

      -- Apply filter to all instances
      return (Set.map f is)

   return (Set.unions os)
