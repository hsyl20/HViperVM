module ViperVM.KernelInterface where

data AccessMode = ReadOnly | ReadWrite

data KernelInterface = KernelInterface {
  name :: String,
  modes :: [AccessMode]
}
