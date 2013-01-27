module ViperVM.TransferManager (
   TransferManager,
   initTransferManager
) where

import ViperVM.Transfer

-- | Manage data transfers
data TransferManager = TransferManager [Transfer]

-- | Initialize a transfer manager
initTransferManager :: TransferManager
initTransferManager = TransferManager []
