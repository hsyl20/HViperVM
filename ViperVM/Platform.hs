{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import ViperVM.Backends.OpenCL.Types
import Data.Traversable
import Data.Word
import Data.Map
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Control.Monad.State
import Data.Lens.Template
import Data.Lens.Lazy

data Platform = Platform {
  _memories :: [Memory],
  _links :: [Link],
  _processors :: [Processor],
  _buffers :: Map Memory [Buffer]
}

data Memory = CLMemory OpenCLLibrary CLDeviceID | HostMemory
data Link = CLLink (Memory,Memory)
data Processor = CLProcessor OpenCLLibrary CLDeviceID
data Buffer = CLBuffer CLMem | HostBuffer Word64 (Ptr ())

$( makeLens ''Platform ) 

instance Eq Memory where
  (==) HostMemory HostMemory = True
  (==) HostMemory _ = False
  (==) _ HostMemory = False
  (==) (CLMemory _ m1) (CLMemory _ m2) = (==) m1 m2

instance Ord Memory where
  compare HostMemory HostMemory = EQ
  compare (CLMemory _ m1) (CLMemory _ m2) = compare m1 m2
  compare HostMemory _ = GT
  compare _ HostMemory = LT

initPlatform :: String -> IO Platform
initPlatform cllib = do
  lib <- loadOpenCL cllib
  platforms <- clGetPlatformIDs lib
  devices <- fmap concat $ traverse (clGetDeviceIDs lib CL_DEVICE_TYPE_ALL) platforms
  let procs = fmap (CLProcessor lib) devices
  let mems = fmap (CLMemory lib) devices
  let ls = fmap (\x -> CLLink (HostMemory,x)) mems

  return $ Platform mems ls procs empty

addBuffer :: Memory -> Buffer -> StateT Platform IO ()
addBuffer mem buf = modify (buffers ^%= modBuffer)
  where
    modBuffer = alter f mem
    f (Just x) = Just (x ++ [buf])
    f Nothing  = Just [buf]

allocBuffer :: Memory -> Word64 -> StateT Platform IO Buffer
allocBuffer HostMemory sz = do
  ptr <- lift $ mallocBytes (fromIntegral sz)
  let buf = HostBuffer sz ptr
  addBuffer HostMemory buf
  lift $ return buf

allocBuffer (CLMemory lib mem) sz = do
  clmem <- lift $ clCreateBuffer lib undefined [] (sz,nullPtr)
  let buf = CLBuffer clmem
  addBuffer (CLMemory lib mem) buf
  lift $ return buf
