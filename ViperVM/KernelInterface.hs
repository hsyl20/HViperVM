module ViperVM.KernelInterface where

data AccessMode = ReadOnly | ReadWrite

data KernelInterface = KernelInterface {
  modes :: [AccessMode]
}
