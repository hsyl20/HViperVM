module ViperVM.Transfer where

import ViperVM.View
import ViperVM.Platform
import ViperVM.Data
import Text.Printf

data Transfer = Transfer Link View View

instance Show Transfer where
  show (Transfer l v1 v2) = printf "Transfer from %s to %s on %s" (show v1) (show v2) (show l)

transferDataInstance :: Link -> DataInstance -> DataInstance -> Transfer
transferDataInstance l (Vector v1) (Vector v2) = Transfer l v1 v2
