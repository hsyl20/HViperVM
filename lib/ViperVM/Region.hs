module ViperVM.Region where

import Data.Word

type Offset = Word64
type Size = Word64
type Padding = Word
type Index = Word64

data Region = Region1D Offset Size |
              Region2D Offset Index Size Padding
                deriving (Eq,Ord,Show)

checkCompatibleRegions :: Region -> Region -> Bool
checkCompatibleRegions (Region1D _ s1) (Region1D _ s2) = s1 == s2
checkCompatibleRegions (Region2D _ idx1 s1 _) (Region2D _ idx2 s2 _) = (s1 == s2) && (idx1 == idx2)
checkCompatibleRegions _ _ = False
