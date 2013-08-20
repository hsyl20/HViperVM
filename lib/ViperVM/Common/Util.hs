module ViperVM.Common.Util (
   roundTo
) where

roundTo :: Integral a => a -> a -> a
roundTo to v = v + (if ms /= 0 then to - ms else 0)
   where ms = mod v to

