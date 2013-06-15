module ViperVM.Platform.TransferResult where

data RegionTransferResult =
     RegionTransferSuccess 
   | RegionTransferError
   deriving (Eq,Ord,Show)

