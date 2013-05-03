module ViperVM.Platform.Primitive where

import Prelude hiding (Float,Double)
import Data.Word

data Primitive = Float | Double
                 deriving (Eq,Ord)

instance Show Primitive where
  show Float = "Float"
  show Double = "Double"

size :: Primitive -> Word64
size Float = 4
size Double = 8
