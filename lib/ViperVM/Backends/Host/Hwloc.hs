{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, CPP #-}

module ViperVM.Backends.Host.Hwloc (
  HWLocLibrary(..),
  loadHWLoc
) where

import System.Posix.DynamicLinker.Template
import System.Posix.DynamicLinker

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Char
import Data.Maybe
import Data.List (stripPrefix)
import Control.Applicative

-- | OpenCL Library module object
data HWLocLibrary = HWLocLibrary {
  libHandle :: DL,
  rawHwLocGetApiVersion :: CUInt
}

instance Eq HWLocLibrary where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord HWLocLibrary where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)

myMod :: String -> String
myMod x = fromJust $ ("hwloc_" ++) . concatMap f <$> stripPrefix "rawHwLoc" x
   where
      f a | isUpper a = ['_', toLower a] 
          | otherwise = [a]

$(makeDynamicLinker ''HWLocLibrary CallConv 'myMod)

loadHWLoc :: String -> IO HWLocLibrary
loadHWLoc lib = loadHWLocLibrary lib [RTLD_NOW,RTLD_LOCAL]
