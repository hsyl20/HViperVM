{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ViperVM.Data where

import Data.Word
import ViperVM.View

data Primitive = PrimFloat | PrimDouble

class DataInstance i d | i -> d where
  instanceId :: i -> Word
  getData :: i -> d


type DataId = Word

class Data a where
  dataId :: a -> DataId

data VectorDesc = VectorDesc Primitive Word

