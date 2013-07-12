module ViperVM.Platform.KernelParameter (
   KernelParameter(..)
) where

import Data.Word
import ViperVM.Platform.Memory

data KernelParameter = 
     IntParam Int
   | WordParam Word
   | FloatParam Float
   | DoubleParam Double
   | BufferParam Buffer
     deriving (Show)

