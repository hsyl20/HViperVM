{-# LANGUAGE LambdaCase #-}

module ViperVM.Platform.Transfer where

import ViperVM.Platform.Region
import ViperVM.Backends.OpenCL
import ViperVM.Platform.Link
import ViperVM.Platform.Buffer
import ViperVM.Platform.Memory
import Control.Monad (void)
import Foreign.Ptr

data TransferResult = TransferSuccess | TransferError
                      deriving (Eq,Ord)

data Transfer = Transfer Link Buffer Region Buffer Region
                deriving (Eq,Ord)

-- | Start a new synchronous transfer
submitTransfer :: Transfer -> IO TransferResult
submitTransfer t = if r then transfer t else return TransferError
   where r = checkTransfer t
   

-- | Check that 
checkTransfer :: Transfer -> Bool
checkTransfer (Transfer l b1 r1 b2 r2) = v
  where
      (lm1,lm2) = linkEndpoints l
      m1 = getBufferMemory b1
      m2 = getBufferMemory b2
      v = lm1 == m1 && lm2 == m2 && checkCompatibleRegions r1 r2

                      
transfer :: Transfer -> IO TransferResult
transfer t = case t of 

  -- Host --> CL, Region1D, Region1D
  (Transfer (CLLink lib cq HostMemory (CLMemory {})) (HostBuffer _ ptr) (Region1D soff sz) (CLBuffer _ _ buf) (Region1D doff _)) -> do
         let srcptr = plusPtr ptr (fromIntegral soff)
         e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
         void $ clWaitForEvents lib [e]
         void $ clReleaseEvent lib e
         return TransferSuccess

  -- CL --> Host, Region1D, Region1D
  (Transfer (CLLink lib cq (CLMemory {}) HostMemory) (CLBuffer _ _ buf) (Region1D soff sz) (HostBuffer _ ptr) (Region1D doff _)) -> do
         let dstptr = plusPtr ptr (fromIntegral doff)
         e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
         void $ clWaitForEvents lib [e]
         void $ clReleaseEvent lib e
         return TransferSuccess

  _ -> return TransferError
