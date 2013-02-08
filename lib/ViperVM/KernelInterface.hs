module ViperVM.KernelInterface where

import ViperVM.Runtime.Data

data KernelParameter = KPReadOnly Data  -- ^ Access a data in read-only mode
               | KPReadWrite Data -- ^ Access the first data in virtual read-write mode. The second data is the result and the first is left unmodified
               | KPAllocate DataDesc  -- ^ Allocate a new data

data KernelInterface = KernelInterface {
  name :: String,                                  -- ^ Kernel identifier (name)
  paramCount :: (Int,Int),                         -- ^ Number of parameters (in, out)
  makeParameters :: [Data] -> [KernelParameter],   -- ^ Create kernel parameters from input data
  makeResult :: [Data] -> [Data]                   -- ^ Filter data to return as kernel results
}
