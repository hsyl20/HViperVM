{-# LANGUAGE LambdaCase #-}
module ViperVM.Runtime.Memory.MetaObjects.Matrix where

import ViperVM.Runtime.Memory.MetaObject

newtype MetaMatrix = MetaMatrix [[MetaObject]]

