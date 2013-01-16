module ViperVM.Region where

import qualified ViperVM.Buffer as Buffer (getMemory,getBufferImpl)
import ViperVM.Buffer (Buffer,BufferImpl)
import ViperVM.Platform

import Data.Word

type Offset = Word64
type Size = Word64
type Padding = Word
type Index = Word64

data Region = Region1D Buffer Offset Size |
            Region2D Buffer Offset Index Size Padding
            deriving (Eq,Ord,Show)

checkCompatibleRegions :: Region -> Region -> Bool
checkCompatibleRegions (Region1D _ _ s1) (Region1D _ _ s2) = s1 == s2
checkCompatibleRegions (Region2D _ _ idx1 s1 _) (Region2D _ _ idx2 s2 _) = (s1 == s2) && (idx1 == idx2)
checkCompatibleRegions _ _ = False

-- | Return buffer of the Region
getBuffer :: Region -> Buffer
getBuffer (Region1D b _ _) = b
getBuffer (Region2D b _ _ _ _) = b

getBufferImpl :: Region -> BufferImpl
getBufferImpl = Buffer.getBufferImpl . getBuffer

getMemory :: Region -> Memory
getMemory = Buffer.getMemory . getBuffer

