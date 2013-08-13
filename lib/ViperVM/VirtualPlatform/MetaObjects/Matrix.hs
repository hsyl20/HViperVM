{-# LANGUAGE LambdaCase #-}
module ViperVM.VirtualPlatform.MetaObjects.Matrix where

import ViperVM.VirtualPlatform.MetaObject

newtype MetaMatrix = MetaMatrix [[MetaObject]]

