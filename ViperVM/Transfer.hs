module ViperVM.Transfer where

import ViperVM.Backends.OpenCL.CommandQueue
import ViperVM.Backends.OpenCL.Event
import ViperVM.View
import ViperVM.Platform
import ViperVM.Buffer

import Foreign.Ptr

data Transfer = Transfer Link View View

checkLink :: Link -> View -> View -> Bool
-- CL --> Host
checkLink (CLLink _ _ m1 HostMemory) (View1D (CLBuffer _ m1b _) _ _) (View1D (HostBuffer _ _) _ _) = (m1 == m1b)
checkLink (CLLink _ _ m1 HostMemory) (View2D (CLBuffer _ m1b _) _ _ _ _) (View2D (HostBuffer _ _) _ _ _ _) = (m1 == m1b)
-- Host --> CL
checkLink (CLLink _ _ HostMemory m1) (View1D (HostBuffer _ _) _ _) (View1D (CLBuffer _ m1b _) _ _) = (m1 == m1b)
checkLink (CLLink _ _ m1 HostMemory) (View2D (HostBuffer _ _) _ _ _ _) (View2D (CLBuffer _ m1b _) _ _ _ _) = (m1 == m1b)
-- CL --> CL
checkLink (CLLink _ _ m1 m2) (View1D (CLBuffer _ m1b _) _ _) (View1D (CLBuffer _ m2b _) _ _) = (m1 == m1b) && (m2 == m2b)
checkLink (CLLink _ _ m1 m2) (View2D (CLBuffer _ m1b _) _ _ _ _) (View2D (CLBuffer _ m2b _) _ _ _ _) = (m1 == m1b) && (m2 == m2b)
-- Default
checkLink _ _ _ = False

-- | Check if a transfer is valid
checkTransfer :: Transfer -> Bool
checkTransfer (Transfer link v1 v2) = (checkCompatibleViews v1 v2) && (checkLink link v1 v2)

-- | Perform transfer synchronously
performTransfer :: Transfer -> IO ()
performTransfer (Transfer link src dst) = do
  case link of
    -- Host --> CL
    CLLink lib cq HostMemory _ -> case (src,dst) of
      (View1D (HostBuffer _ ptr) soff sz, View1D (CLBuffer _ _ buf) doff _) -> do
        let srcptr = plusPtr ptr (fromIntegral soff)
        e <- clEnqueueWriteBuffer lib cq buf True doff sz srcptr []
        _ <- clReleaseEvent lib e
        return ()
    -- CL --> Host
    CLLink lib cq _ HostMemory -> case (src,dst) of
      (View1D (CLBuffer _ _ buf) soff sz, View1D (HostBuffer _ ptr) doff _) -> do
        let dstptr = plusPtr ptr (fromIntegral doff)
        e <- clEnqueueReadBuffer lib cq buf True soff sz dstptr []
        _ <- clReleaseEvent lib e
        return ()
