module ViperVM.KernelInterface where

import ViperVM.Data

data KernelParameter = KPReadOnly Data  -- ^ Access a data in read-only mode
               | KPReadWrite Data -- ^ Access the first data in virtual read-write mode. The second data is the result and the first is left unmodified
               | KPAllocate DataDesc  -- ^ Allocate a new data

data KernelInterface = KernelInterface {
  -- | Kernel identifier (name)
  name :: String,
  -- | Create kernel parameters from input data
  makeParameters :: [Data] -> [KernelParameter],
  -- | Filter data to return as kernel results
  makeResult :: [Data] -> [Data]
}
