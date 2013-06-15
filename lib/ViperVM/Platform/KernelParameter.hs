module ViperVM.Platform.KernelParameter (
   KernelParameter(..)
) where

import Data.Word
import ViperVM.Platform.Buffer

data KernelParameter = 
     IntParam Int
   | WordParam Word
   | FloatParam Float
   | DoubleParam Double
   | BufferParam Buffer
     deriving (Show)

