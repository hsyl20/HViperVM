module ViperVM.KernelInterface where

import ViperVM.Data

data Parameter = ReadOnly Data  -- ^ Access a data in read-only mode
               | ReadWrite Data -- ^ Access the first data in virtual read-write mode. The second data is the result and the first is left unmodified
               | Allocate DataDesc  -- ^ Allocate a new data

data KernelInterface = KernelInterface {
  -- | Kernel identifier (name)
  name :: String,
  -- | Create kernel parameters from input data
  makeParameters :: [Data] -> [Parameter],
  -- | Filter data to return as kernel results
  makeResult :: [Data] -> [Data]
}
