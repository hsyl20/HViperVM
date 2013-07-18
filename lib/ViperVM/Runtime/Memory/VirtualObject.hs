{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}

module ViperVM.Runtime.Memory.VirtualObject (
   Descriptor(..), VirtualObject, descriptor, instances,
   createVirtualObject, attachInstance, detachInstance,
   exchangeInstance, instanceMemories, instancesInMemory,
   subLinkTo, allInstances, linkedInstancesInMemory,
   LinkType(..), LinkIdx(..)
) where

import ViperVM.Runtime.Memory.Object
import qualified ViperVM.Runtime.Memory.Objects.Vector as Vector
import qualified ViperVM.Runtime.Memory.Objects.Matrix as Matrix
import ViperVM.Platform.Memory
import ViperVM.STM.TSet as TSet
import ViperVM.Platform.Primitive as Prim

import Data.Typeable
import Data.Word
import Data.Set as Set
import Control.Concurrent.STM
import Control.Applicative
import Data.Traversable (forM)

-- | Virtual object
-- Set of identical objects stored in different memories
data VirtualObject = VirtualObject {
   soID :: Word64,
   descriptor :: Descriptor,
   objects :: TSet Object,
   parents :: TSet SubLink,
   children :: TSet VirtualObject
} deriving (Typeable)

instance Eq VirtualObject where
   (==) a b = soID a == soID b

instance Ord VirtualObject where
   compare a b = compare (soID a) (soID b)

instance Show VirtualObject where
   show so = "ShObj(" ++ show (descriptor so) ++ ")"

data LinkType = MatrixSplit Word64 Word64 deriving (Eq,Ord)
data LinkIdx = MatrixSplitIdx Int Int deriving (Eq,Ord)
data SubLink = SubLink VirtualObject LinkType LinkIdx deriving (Eq,Ord)

-- | Create a shared object
createVirtualObject :: Word64 -> Descriptor -> STM VirtualObject
createVirtualObject identifier desc = do
   VirtualObject identifier desc <$> TSet.empty <*> TSet.empty <*> TSet.empty

-- | Return objects associated to this shared-object
instances :: VirtualObject -> STM (Set Object)
instances = readTVar . objects

-- | Attach an object to a shared object 
-- Attached object must be compatible with shared object descriptor
attachInstance :: VirtualObject -> Object -> STM ()
attachInstance so o = do

   if checkObject (descriptor so) o
      then TSet.insert o (objects so)
      else error "Fail"


-- | Detach an object from a shared object
detachInstance :: VirtualObject -> Object -> STM ()
detachInstance so o = do
   TSet.delete o (objects so)

-- | Exchange an object instance between two shared objects
exchangeInstance :: Object -> VirtualObject -> VirtualObject -> STM ()
exchangeInstance o src dst = do
   detachInstance src o
   attachInstance dst o

-- | Retrieve memories into which an instance of the object exists
instanceMemories :: VirtualObject -> STM (Set Memory)
instanceMemories so = Set.map objectMemory <$> instances so

-- | Retrieve object instances in the given memory
instancesInMemory :: VirtualObject -> Memory -> STM (Set Object)
instancesInMemory so mem = Set.filter f <$> instances so
   where
      f = (==) mem . objectMemory 

-- | Retrieve object instances in the given memory
linkedInstancesInMemory :: VirtualObject -> Memory -> STM (Set Object)
linkedInstancesInMemory so mem = Set.filter f <$> linkedInstances so
   where
      f = (==) mem . objectMemory 

-- | Attach a parent data
subLinkTo :: VirtualObject -> LinkType -> LinkIdx -> VirtualObject -> STM ()
subLinkTo child linkType linkIdx parent = do
   let subLink = SubLink parent linkType linkIdx
   TSet.insert subLink (parents child) 
   TSet.insert child (children parent) 

-- | Retrieve all instances of a shared object
allInstances :: VirtualObject -> STM (Set Object)
allInstances so = Set.union <$> instances so <*> linkedInstances so

-- | Retrieve instances from linked objects
linkedInstances :: VirtualObject -> STM (Set Object)
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

-- | Allocate a compatible instance of the shared object, DO NOT attach it
allocateInstance :: VirtualObject -> Memory -> IO Object
allocateInstance o mem = allocateFromDescriptor mem (descriptor o)

-- | Allocate a compatible instance of the shared object, DO NOT atach it
allocateFromDescriptor :: Memory -> Descriptor -> IO Object
allocateFromDescriptor mem (MatrixDesc prim w h) = do
   let padding = (w * Prim.sizeOf prim) `mod` 4
   allocateMatrix om mem prim w h padding >>= \case
      Nothing -> error "Unable to allocate matrix"
      Just m -> return (MatrixObject m)

allocateFromDescriptor mem (VectorDesc prim sz) = do
   allocateVector om mem prim sz >>= \case
      Nothing -> error "Unable to allocate vector"
      Just v -> return (VectorObject v)

-- | Release an instance of the shared object
releaseInstance :: VirtualMemory -> VirtualObject -> Object -> IO ()
releaseInstance som so o = do
   let om = objectManager som
   atomically (detachInstance so o)
   releaseObject om o


-- | Allocate a new instance, transfer appropriate data from another one, then
-- associate the new instance
allocateTransferAttach :: VirtualMemory -> VirtualObject -> Object -> Memory -> IO Object
allocateTransferAttach som so src dstMem = do
   let 
      om = objectManager som
      srcMem = objectMemory src
      pf = virtualMemoryPlatform som
      lks = linksBetween srcMem dstMem (links pf)
      indirectTransfer = Prelude.null lks

   -- Allocate destination
   dst <- allocateInstance som so dstMem


   -- Intermediate steps
   steps <- if indirectTransfer
      then do
         -- Use host memory as intermediate
         let hostMem = head . hostMemories $ virtualMemoryPlatform som
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
ensureInMemory :: VirtualMemory -> Memory -> VirtualObject -> IO Object
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
