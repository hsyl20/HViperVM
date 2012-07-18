{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import Data.Traversable
import Data.Functor
import Data.Lens.Template

data Platform = Platform {
  _memories :: [Memory],
  _links :: [Link],
  _processors :: [Processor]
}

data Memory = CLMemory OpenCLLibrary CLContext CLDeviceID | HostMemory
data Link = CLLink OpenCLLibrary Memory Memory
data Processor = CLProcessor OpenCLLibrary CLContext CLDeviceID

$( makeLens ''Platform ) 

instance Eq Memory where
  (==) HostMemory HostMemory = True
  (==) (CLMemory _ _ m1) (CLMemory _ _ m2) = (==) m1 m2
  (==) _ _ = False

instance Ord Memory where
  compare HostMemory HostMemory = EQ
  compare (CLMemory _ _ m1) (CLMemory _ _ m2) = compare m1 m2
  compare HostMemory _ = GT
  compare _ HostMemory = LT

-- | Initialize platform
initPlatform :: String -> IO Platform
initPlatform cllib = do
  lib <- loadOpenCL cllib
  platforms <- clGetPlatformIDs lib
  devices <- traverse (clGetDeviceIDs lib CL_DEVICE_TYPE_ALL) platforms
  let platformDevices = zip platforms devices
  contexts <- traverse (\(pf,devs) -> clCreateContext lib [CL_CONTEXT_PLATFORM pf] devs putStrLn) platformDevices
  let deviceContexts = zip devices contexts
  let procs = concat $ (\(devs,ctx) -> fmap (CLProcessor lib ctx) devs) <$> deviceContexts
  let mems  = concat $ (\(devs,ctx) -> fmap (CLMemory lib ctx) devs) <$> deviceContexts
  let ls = fmap (CLLink lib HostMemory) mems
  return $ Platform mems ls procs
