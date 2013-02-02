module ViperVM.Transfer where

import ViperVM.Buffer
import ViperVM.Region
import ViperVM.Platform
import ViperVM.Data
import Text.Printf

data Transfer = Transfer Link Buffer Region Buffer Region

instance Show Transfer where
  show (Transfer l b1 v1 b2 v2) = printf "Transfer from %s of buffer %d to %s of buffer %d through %s" (show v1) (show b1) (show v2) (show b2) (show l)

transferDataInstance :: Link -> DataInstance -> DataInstance -> Transfer
transferDataInstance l (Vector b1 v1) (Vector b2 v2) = Transfer l b1 v1 b2 v2
