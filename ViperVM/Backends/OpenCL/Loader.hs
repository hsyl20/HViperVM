{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module ViperVM.Backends.OpenCL.Loader (
  OpenCLLibrary(..),
  loadOpenCL,
) where

import Prelude hiding (catch)

import System.Posix.DynamicLinker
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Maybe (fromMaybe)
import Data.Map
import Data.Word
import Data.Traversable

import ViperVM.Backends.OpenCL.Types

-- | List of OpenCL library symbols
openCLSymbols :: [String]
openCLSymbols = [
  "clGetPlatformIDs",
  "clGetPlatformInfo",
  "clGetDeviceIDs",
  "clGetDeviceInfo",
  "clCreateContext",
  "clCreateContextFromType",
  "clRetainContext",
  "clReleaseContext",
  "clGetContextInfo",
  "clCreateCommandQueue",
  "clRetainCommandQueue",
  "clReleaseCommandQueue",
  "clGetCommandQueueInfo",
  "clSetCommandQueueProperty",
  "clCreateBuffer",
  "clCreateSubBuffer",
  "clCreateImage2D",
  "clCreateImage3D",
  "clRetainMemObject",
  "clReleaseMemObject",
  "clGetSupportedImageFormats",
  "clGetMemObjectInfo",
  "clGetImageInfo",
  "clSetMemObjectDestructorCallback",
  "clCreateSampler",
  "clRetainSampler",
  "clReleaseSampler",
  "clGetSamplerInfo",
  "clCreateProgramWithSource",
  "clCreateProgramWithBinary",
  "clRetainProgram",
  "clReleaseProgram",
  "clBuildProgram",
  "clUnloadCompiler",
  "clGetProgramInfo",
  "clGetProgramBuildInfo",
  "clCreateKernel",
  "clCreateKernelsInProgram",
  "clRetainKernel",
  "clReleaseKernel",
  "clSetKernelArg",
  "clGetKernelInfo",
  "clGetKernelWorkgroupInfo",
  "clWaitForEvents",
  "clGetEventInfo",
  "clCreateUserEvent",
  "clRetainEvent",
  "clReleaseEvent",
  "clSetUserEventStatus",
  "clSetEventCallback",
  "clGetEventProfilingInfo",
  "clFlush",
  "clFinish",
  "clEnqueueReadBuffer",
  "clEnqueueReadBufferRect",
  "clEnqueueWriteBuffer",
  "clEnqueueWriteBufferRect",
  "clEnqueueCopyBuffer",
  "clEnqueueCopyBufferRect",
  "clEnqueueReadImage",
  "clEnqueueWriteImage",
  "clEnqueueCopyImage",
  "clEnqueueCopyImageToBuffer",
  "clEnqueueMapBuffer",
  "clEnqueueMapImage",
  "clEnqueueUnmapMemObject",
  "clEnqueueNDRangeKernel",
  "clEnqueueTask",
  "clEnqueueNativeKernel",
  "clEnqueueMarker",
  "clEnqueueWaitForEvents",
  "clEnqueueBarrier",
  "clGetExtensionFunctionAddress"
  ]

type GetPlatformIDsFun = CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint
type GetPlatformInfoFun = CLPlatformID -> CLPlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
type GetDeviceIDsFun = CLPlatformID -> CLDeviceType_ -> CLuint -> Ptr CLDeviceID -> Ptr CLuint -> IO CLint
type GetDeviceInfoFun = CLDeviceID -> CLDeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

data OpenCLLibrary = OpenCLLibrary {
  libHandle :: DL,
  raw_clGetPlatformIDs    :: GetPlatformIDsFun,
  raw_clGetPlatformInfo   :: GetPlatformInfoFun,
  raw_clGetDeviceIDs      :: GetDeviceIDsFun,
  raw_clGetDeviceInfo     :: GetDeviceInfoFun
}

foreign import CALLCONV "dynamic" mkGetPlatformIDsFun :: FunPtr GetPlatformIDsFun -> GetPlatformIDsFun
foreign import CALLCONV "dynamic" mkGetPlatformInfoFun :: FunPtr GetPlatformInfoFun -> GetPlatformInfoFun
foreign import CALLCONV "dynamic" mkGetDeviceIDsFun :: FunPtr GetDeviceIDsFun -> GetDeviceIDsFun
foreign import CALLCONV "dynamic" mkGetDeviceInfoFun :: FunPtr GetDeviceInfoFun -> GetDeviceInfoFun

-- | dlsym without exception (may return nullFunPtr)
mydlsym :: DL -> String -> IO (FunPtr a)
mydlsym source symbol = do
  withCAString symbol $ \ s -> do
    c_dlsym (packDL source) s

-- | Load the given list of symbols from the given library handle
loadSymbols :: DL -> [String] -> IO (Map String (FunPtr a))
loadSymbols dl symbols = do
  ptrs <- traverse (mydlsym dl) symbols
  return $ fromList (symbols `zip` ptrs)

-- | Load symbols of an OpenCL library
loadOpenCL :: String -> IO OpenCLLibrary
loadOpenCL lib = do
  dl <- dlopen lib [RTLD_NOW,RTLD_LOCAL]
  symPtrs <- loadSymbols dl openCLSymbols

  let notFound a = error ("Mandatory OpenCL function \"" ++ a ++ "\" not found")
  let pick a = castFunPtr $ fromMaybe nullFunPtr (Data.Map.lookup a symPtrs)
  let mandatory a = if (pick a == nullFunPtr) then notFound a else pick a

  return OpenCLLibrary {
    libHandle = dl,
    raw_clGetPlatformIDs = mkGetPlatformIDsFun $ mandatory "clGetPlatformIDs",
    raw_clGetPlatformInfo = mkGetPlatformInfoFun $ mandatory "clGetPlatformInfo",
    raw_clGetDeviceIDs = mkGetDeviceIDsFun $ mandatory "clGetDeviceIDs",
    raw_clGetDeviceInfo = mkGetDeviceInfoFun $ mandatory "clGetDeviceInfo"
  }

