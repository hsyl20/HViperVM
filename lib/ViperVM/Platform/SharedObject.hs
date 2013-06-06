module ViperVM.Platform.SharedObject (
   Descriptor(..), SharedObject, descriptor,
   createSharedObject, attachObject, detachObject,
   exchangeObject, objectMemories
) where

import ViperVM.Platform.Object
import ViperVM.Platform.Memory
import ViperVM.STM.TSet as TSet
import ViperVM.Platform.Primitive

import Data.Word
import Data.Set as Set
import Control.Concurrent.STM
import Control.Applicative

-- | Descriptor of a shared object
data Descriptor = VectorDesc Primitive Word64         -- ^ Vector
                | MatrixDesc Primitive Word64 Word64  -- ^ Matrix
                deriving (Eq,Ord,Show)

-- | Shared object
-- Set of identical objects stored in different memories
data SharedObject = SharedObject {
      descriptor :: Descriptor,
      objects :: TSet Object
   }

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
objectMemories so = Set.map objectMemory <$> readTVar (objects so)
