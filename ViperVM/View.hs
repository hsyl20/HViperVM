module ViperVM.View where

import ViperVM.Platform
import ViperVM.Buffer

import Control.Monad.State
import Data.Word

type Offset = Word64
type Size = Word64
type Padding = Word
type Index = Word64

data View = View1D Buffer Offset Size |
            View2D Buffer Offset Index Size Padding

checkCompatibleViews :: View -> View -> Bool
checkCompatibleViews (View1D _ _ s1) (View1D _ _ s2) = s1 == s2
checkCompatibleViews (View2D _ _ idx1 s1 _) (View2D _ _ idx2 s2 _) = (s1 == s2) && (idx1 == idx2)
checkCompatibleViews _ _ = False

checkLink :: Link -> View -> View -> Bool
-- CL --> Host
checkLink (CLLink _ m1 HostMemory) (View1D (CLBuffer _ m1b _) _ _) (View1D (HostBuffer _ _) _ _) = (m1 == m1b)
checkLink (CLLink _ m1 HostMemory) (View2D (CLBuffer _ m1b _) _ _ _ _) (View2D (HostBuffer _ _) _ _ _ _) = (m1 == m1b)
-- Host --> CL
checkLink (CLLink _ HostMemory m1) (View1D (HostBuffer _ _) _ _) (View1D (CLBuffer _ m1b _) _ _) = (m1 == m1b)
checkLink (CLLink _ m1 HostMemory) (View2D (HostBuffer _ _) _ _ _ _) (View2D (CLBuffer _ m1b _) _ _ _ _) = (m1 == m1b)
-- CL --> CL
checkLink (CLLink _ m1 m2) (View1D (CLBuffer _ m1b _) _ _) (View1D (CLBuffer _ m2b _) _ _) = (m1 == m1b) && (m2 == m2b)
checkLink (CLLink _ m1 m2) (View2D (CLBuffer _ m1b _) _ _ _ _) (View2D (CLBuffer _ m2b _) _ _ _ _) = (m1 == m1b) && (m2 == m2b)
-- Default
checkLink _ _ _ = False

-- | Check if a transfer is valid
checkTransfer :: Link -> View -> View -> Bool
checkTransfer link v1 v2 = (checkCompatibleViews v1 v2) && (checkLink link v1 v2)

-- | Perform an asynchronous transfer
viewTransfer :: Link -> View -> View -> StateT Platform IO ()
viewTransfer link v1 v2 = do
  lift $ if not (checkTransfer link v1 v2) then error "Invalid transfer" else return ()
