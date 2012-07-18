{-# LANGUAGE TemplateHaskell, FlexibleContexts, ScopedTypeVariables #-} 

module ViperVM.Platform where

import ViperVM.Backends.OpenCL
import ViperVM.Backends.OpenCL.Types
import Data.Traversable
import Data.Word
import Data.Map
import qualified Data.List as List
import Data.Functor
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

data Memory = CLMemory OpenCLLibrary CLContext CLDeviceID | HostMemory
data Link = CLLink OpenCLLibrary Memory Memory
data Processor = CLProcessor OpenCLLibrary CLContext CLDeviceID
data Buffer = CLBuffer Memory CLMem | HostBuffer Word64 (Ptr ())

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

instance Eq Buffer where
  (==) (CLBuffer mem1 m1) (CLBuffer mem2 m2) = (mem1 == mem2) && (m1 == m2)
  (==) (HostBuffer _ m1) (HostBuffer _ m2) = m1 == m2
  (==) _ _ = False

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
  return $ Platform mems ls procs empty

-- | Register a new buffer
registerBuffer :: Memory -> Buffer -> StateT Platform IO ()
registerBuffer mem buf = modify (buffers ^%= modBuffer)
  where
    modBuffer = alter f mem
    f (Just x) = Just (x ++ [buf])
    f Nothing  = Just [buf]

-- | Unregister a buffer
unregisterBuffer :: Memory -> Buffer -> StateT Platform IO ()
unregisterBuffer mem buf = modify (buffers ^%= modBuffer)
  where
    modBuffer :: Map Memory [Buffer] -> Map Memory [Buffer]
    modBuffer = alter (fmap $ List.delete buf) mem


-- | Allocate a buffer in the given memory
allocBuffer :: Memory -> Word64 -> StateT Platform IO Buffer
allocBuffer mem sz = do
  buf <- case mem of

    HostMemory -> do
      ptr <- lift $ mallocBytes (fromIntegral sz)
      return $ HostBuffer sz ptr

    (CLMemory lib ctx dev) -> do
      clmem <- lift $ clCreateBuffer lib ctx [] (sz,nullPtr)
      -- TODO: Force allocation on dev
      return $ CLBuffer mem clmem

  registerBuffer mem buf
  lift $ return buf

-- | Release a buffer
releaseBuffer :: Buffer -> StateT Platform IO ()
releaseBuffer buf = do
  mem <- case buf of

    (HostBuffer _ ptr) -> do
      lift $ free ptr
      return HostMemory

    (CLBuffer mem m) -> do
      let (CLMemory lib _ _) = mem
      _ <- lift $ clReleaseMemObject lib m
      return mem

  unregisterBuffer mem buf
