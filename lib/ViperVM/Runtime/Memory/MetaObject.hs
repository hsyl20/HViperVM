{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}
module ViperVM.Runtime.Memory.MetaObject (
   MetaObject(..), allInstances, 
   allocate, attachInstance, detachInstance, exchangeInstance,
   instanceMemories, instancesInMemory, allocateInstance
) where

import ViperVM.Runtime.Memory.Object
import ViperVM.Runtime.Memory.Descriptor
import ViperVM.STM.TSet as TSet
import ViperVM.Platform.Memory

import Control.Concurrent.STM
import Control.Applicative ((<$>), (<*>))
import Data.Typeable
import Data.Traversable (forM)
import Data.Word
import Data.Set as Set
import System.Clock

-- | Meta object
-- Set of identical objects stored in different memories
data MetaObject = MetaObject {
   metaObjectID :: Word64,       -- ^ Unique identifier
   descriptor :: Descriptor,     -- ^ High-level data description
   objects :: TSet Object,       -- ^ Raw objects
   subObjects :: TSet SubObject  -- ^ Objects coming from other meta objects
} deriving (Typeable)

instance Eq MetaObject where
   (==) a b = metaObjectID a == metaObjectID b

instance Ord MetaObject where
   compare a b = compare (metaObjectID a) (metaObjectID b)

instance Show MetaObject where
   show so = "ShObj(" ++ show (descriptor so) ++ ")"

type ObjectFilter = Object -> Object
data SubObject = SubObject MetaObject ObjectFilter


-- | Allocate a meta object
allocate :: Descriptor -> IO MetaObject
allocate desc = do
   tim <- getTime Monotonic -- FIXME: use an additional random generator per thread
   let oid = fromIntegral (nsec tim) + 1000000000 * fromIntegral (sec tim)
   atomically (MetaObject oid desc <$> TSet.empty <*> TSet.empty)

-- | Retrieve all object instances
allInstances :: MetaObject -> STM (Set Object)
allInstances mo = Set.union <$> instances mo <*> indirectInstances mo

-- | Direct object instances
instances :: MetaObject -> STM (Set Object)
instances = readTVar . objects

-- | Indirect object instances
indirectInstances :: MetaObject -> STM (Set Object)
indirectInstances mo = do
   sos <- TSet.elems (subObjects mo)
   Set.unions <$> forM sos link2Instances

   where
      link2Instances (SubObject smo f) = Set.map f <$> allInstances smo


-- | Attach an object to a meta object 
-- Attached object must be compatible with shared object descriptor
attachInstance :: MetaObject -> Object -> STM ()
attachInstance mo o = do

   if checkObject (descriptor mo) (objectPeer o)
      then TSet.insert o (objects mo)
      else error "Fail"


-- | Detach an object from a shared object
detachInstance :: MetaObject -> Object -> STM ()
detachInstance mo o = do
   TSet.delete o (objects mo)

-- | Exchange an object instance between two shared objects
exchangeInstance :: Object -> MetaObject -> MetaObject -> STM ()
exchangeInstance o src dst = do
   detachInstance src o
   attachInstance dst o

-- | Retrieve memories into which an instance of the object exists
instanceMemories :: MetaObject -> STM (Set Memory)
instanceMemories so = Set.map objectMemory <$> instances so

-- | Retrieve object instances in the given memory
instancesInMemory :: MetaObject -> Memory -> STM (Set Object)
instancesInMemory mo mem = Set.filter f <$> instances mo
   where
      f = (==) mem . objectMemory 


-- | Allocate a compatible instance of the shared object, DO NOT attach it
allocateInstance :: MetaObject -> Memory -> IO Object
allocateInstance o mem = allocateFromDescriptor mem (descriptor o)

