{-# LANGUAGE LambdaCase #-}
module ViperVM.Runtime.Memory.Descriptor where

import ViperVM.Runtime.Memory.Object
import ViperVM.Runtime.Memory.Objects.Vector
import ViperVM.Runtime.Memory.Objects.Matrix
import ViperVM.Platform.Primitive
import ViperVM.Platform.Memory
import Data.Word

-- | Descriptor of an object
data Descriptor = 
     VectorDesc Primitive Word64         -- ^ Vector
   | MatrixDesc Primitive Word64 Word64  -- ^ Matrix
   deriving (Eq,Ord,Show)


-- | Allocate a compatible instance of the shared object, DO NOT attach it
allocateFromDescriptor :: Memory -> Descriptor -> IO Object
allocateFromDescriptor mem (MatrixDesc prim w h) = do
   let padding = (w * sizeOf prim) `mod` 4
   matrixAllocate mem prim w h padding >>= \case
      Nothing -> error "Unable to allocate matrix"
      Just m -> initObject (MatrixObject m) mem

allocateFromDescriptor mem (VectorDesc prim sz) = do
   vectorAllocate mem prim sz >>= \case
      Nothing -> error "Unable to allocate vector"
      Just v -> initObject (VectorObject v) mem



matrixDescDims :: Descriptor -> (Word64,Word64)
matrixDescDims (MatrixDesc _ w h) = (w,h)
matrixDescDims _ = error "Not a matrix descriptor"

matrixDescWidth :: Descriptor -> Word64
matrixDescWidth = fst . matrixDescDims

matrixDescHeight :: Descriptor -> Word64
matrixDescHeight = snd . matrixDescDims

matrixDescCellType :: Descriptor -> Primitive
matrixDescCellType (MatrixDesc p _ _) = p
matrixDescCellType _ = error "Not a matrix descriptor"

checkObject :: Descriptor -> ObjectPeer -> Bool

checkObject (VectorDesc p sz) (VectorObject v) =
   vectorCellType v == p && vectorSize v == sz

checkObject (MatrixDesc p w h) (MatrixObject m) =
   matrixCellType m == p && matrixWidth m == w && matrixHeight m == h

checkObject _ _ = False
