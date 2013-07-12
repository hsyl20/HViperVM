module ViperVM.Platform.Primitive where

import Prelude hiding (Float,Double)
import Data.Word

data Primitive = Float | Double
                 deriving (Eq,Ord)

instance Show Primitive where
  show Float = "Float"
  show Double = "Double"

sizeOf :: Primitive -> Word64
sizeOf Float = 4
sizeOf Double = 8
