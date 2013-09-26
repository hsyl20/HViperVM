module ViperVM.Backends.Common.Buffer where

data AllocResult a =
     AllocSuccess a
   | AllocError


instance Functor AllocResult where
   fmap f (AllocSuccess x) = AllocSuccess (f x)
   fmap _ AllocError = AllocError
