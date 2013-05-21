module ViperVM.Platform.SharedObjectManager (
   SharedObjectManager,
   createSharedObjectManager, 
   attachObject, detachObject
) where

import ViperVM.Platform.Object
import ViperVM.Platform.SharedObject
import ViperVM.STM.TSet as TSet
import ViperVM.Platform.ObjectManager

import Control.Concurrent.STM
import Control.Applicative

data SharedObject = SharedObject Descriptor (TSet Object)

data SharedObjectManager = SharedObjectManager {
   objectManager :: ObjectManager,
   sharedObjects :: TSet SharedObject
}

createSharedObjectManager :: ObjectManager -> IO SharedObjectManager
createSharedObjectManager om = do
   so <- atomically $ TSet.empty
   return (SharedObjectManager om so)

allocateSharedObject :: SharedObjectManager -> Descriptor -> STM SharedObject
allocateSharedObject som desc = do
   so <- SharedObject desc <$> TSet.empty
   --TSet.insert so (sharedObjects som)
   --TODO
   return so

attachObject :: SharedObjectManager -> SharedObject -> Object -> STM ()
attachObject _ so o = do
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


detachObject :: SharedObjectManager -> SharedObject -> Object -> STM ()
detachObject _ so o = do
   let SharedObject _ objs = so
   TSet.delete o objs
