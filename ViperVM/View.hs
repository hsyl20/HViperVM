module ViperVM.View where

import ViperVM.Buffer

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
