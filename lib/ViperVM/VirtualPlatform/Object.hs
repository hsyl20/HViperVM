{-# LANGUAGE DeriveDataTypeable #-}

module ViperVM.VirtualPlatform.Object (
   Object(..), ObjectPeer(..), initObject, directTransfer,
   initSubObject,
   lockObject, unlockObject,
   lockObjectIO, unlockObjectIO
) where

import ViperVM.Platform.Memory
import ViperVM.Platform.Link
import ViperVM.VirtualPlatform.Objects.Vector
import ViperVM.VirtualPlatform.Objects.Matrix

import Control.Concurrent.STM
import Data.Typeable
import Control.Applicative ( (<$>) )
import Control.Monad (when)

data ObjectPeer = 
     VectorObject Vector   -- ^ Vector object
   | MatrixObject Matrix   -- ^ Matrix object
   deriving (Show,Eq,Ord)


-- | An object in a memory
data Object = Object {
   objectPeer :: ObjectPeer,
   objectMemory :: Memory,
   locking :: TVar Bool
} deriving (Typeable)


instance Eq Object where
   (==) a b = objectPeer a == objectPeer b

instance Ord Object where
   compare a b = compare (objectPeer a) (objectPeer b)

instance Show Object where
   show = show . objectPeer

initObject :: ObjectPeer -> Memory -> IO Object
initObject peer mem = Object peer mem <$> newTVarIO False

-- | Initialize an object with the lock mechanism shared with its parent
initSubObject :: (ObjectPeer -> ObjectPeer) -> Object -> Object
initSubObject f obj = Object peer mem lck
   where
      peer = f (objectPeer obj)
      mem = objectMemory obj
      lck = locking obj

-- | Lock an object in read-only mode
lockObject :: Object -> STM ()
lockObject obj = do
   v <- readTVar (locking obj)
   when v retry
   writeTVar (locking obj) True

-- | Unlock an object
unlockObject :: Object -> STM ()
unlockObject obj = do
   writeTVar (locking obj) False

lockObjectIO :: Object -> IO ()
lockObjectIO = atomically . lockObject

unlockObjectIO :: Object -> IO ()
unlockObjectIO = atomically . unlockObject


-- | Perform a transfer
directTransfer :: Link -> Object -> Object -> IO ()
directTransfer link src dst = do
   let srcP = objectPeer src
       dstP = objectPeer dst

   atomically $ do
      lockObject src
      lockObject dst

   case (srcP,dstP) of
      (VectorObject v1, VectorObject v2) -> vectorTransfer link v1 v2
      (MatrixObject m1, MatrixObject m2) -> matrixTransfer link m1 m2
      _ -> error "Invalid transfer"

   atomically $ do
      unlockObject src
      unlockObject dst
