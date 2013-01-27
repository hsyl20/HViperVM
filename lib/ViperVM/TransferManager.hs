module ViperVM.TransferManager (
   TransferManager,
   ViperVM.TransferManager.init
) where

import ViperVM.Transfer

-- | Manage data transfers
data TransferManager = TransferManager [Transfer]

-- | Initialize a transfer manager
init :: TransferManager
init = TransferManager []
