{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module ViperVM.Backends.OpenCL.Loader (
  OpenCLLibrary(..),
  loadOpenCL,
) where

import Prelude hiding (catch)

import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Maybe (fromMaybe)
import Data.Map
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
type CreateBufferFun = CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
type CreateImage2DFun = CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
type CreateImage3DFun = CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem

type CreateFromGLTexture2DFun = CLContext -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> Ptr CLint -> IO CLMem
type CreateFromGLBufferFun = CLContext -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO CLMem
type RetainMemObjectFun = CLMem -> IO CLint
type ReleaseMemObjectFun = CLMem -> IO CLint
type GetSupportedImageFormatsFun = CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p -> Ptr CLuint -> IO CLint
type GetMemObjectInfoFun = CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
type GetImageInfoFun = CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
type CreateSamplerFun = CLContext -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO CLSampler
type RetainSamplerFun = CLSampler -> IO CLint
type ReleaseSamplerFun = CLSampler -> IO CLint
type GetSamplerInfoFun = CLSampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint


data OpenCLLibrary = OpenCLLibrary {
  libHandle :: DL,
  raw_clGetPlatformIDs    :: GetPlatformIDsFun,
  raw_clGetPlatformInfo   :: GetPlatformInfoFun,
  raw_clGetDeviceIDs      :: GetDeviceIDsFun,
  raw_clGetDeviceInfo     :: GetDeviceInfoFun,
  raw_clCreateBuffer      :: CreateBufferFun,
  raw_clCreateImage2D     :: CreateImage2DFun,
  raw_clCreateImage3D     :: CreateImage3DFun,
  raw_clCreateFromGLTexture2D :: CreateFromGLTexture2DFun,
  raw_clCreateFromGLBuffer :: CreateFromGLBufferFun,
  raw_clRetainMemObject   :: RetainMemObjectFun,
  raw_clReleaseMemObject  :: ReleaseMemObjectFun,
  raw_clGetSupportedImageFormats :: GetSupportedImageFormatsFun,
  raw_clGetMemObjectInfo  :: GetMemObjectInfoFun,
  raw_clGetImageInfo      :: GetImageInfoFun,
  raw_clCreateSampler     :: CreateSamplerFun,
  raw_clRetainSampler     :: RetainSamplerFun,
  raw_clReleaseSampler    :: ReleaseSamplerFun,
  raw_clGetSamplerInfo    :: GetSamplerInfoFun
}

foreign import CALLCONV "dynamic" mkGetPlatformIDsFun :: FunPtr GetPlatformIDsFun -> GetPlatformIDsFun
foreign import CALLCONV "dynamic" mkGetPlatformInfoFun :: FunPtr GetPlatformInfoFun -> GetPlatformInfoFun
foreign import CALLCONV "dynamic" mkGetDeviceIDsFun :: FunPtr GetDeviceIDsFun -> GetDeviceIDsFun
foreign import CALLCONV "dynamic" mkGetDeviceInfoFun :: FunPtr GetDeviceInfoFun -> GetDeviceInfoFun
foreign import CALLCONV "dynamic" mkCreateBufferFun :: FunPtr CreateBufferFun -> CreateBufferFun
foreign import CALLCONV "dynamic" mkCreateImage2DFun :: FunPtr CreateImage2DFun -> CreateImage2DFun
foreign import CALLCONV "dynamic" mkCreateImage3DFun :: FunPtr CreateImage3DFun -> CreateImage3DFun
foreign import CALLCONV "dynamic" mkCreateFromGLTexture2DFun :: FunPtr CreateFromGLTexture2DFun -> CreateFromGLTexture2DFun
foreign import CALLCONV "dynamic" mkCreateFromGLBufferFun :: FunPtr CreateFromGLBufferFun -> CreateFromGLBufferFun
foreign import CALLCONV "dynamic" mkRetainMemObjectFun :: FunPtr RetainMemObjectFun -> RetainMemObjectFun
foreign import CALLCONV "dynamic" mkReleaseMemObjectFun :: FunPtr ReleaseMemObjectFun -> ReleaseMemObjectFun
foreign import CALLCONV "dynamic" mkGetSupportedImageFormatsFun :: FunPtr GetSupportedImageFormatsFun -> GetSupportedImageFormatsFun
foreign import CALLCONV "dynamic" mkGetMemObjectInfoFun :: FunPtr GetMemObjectInfoFun -> GetMemObjectInfoFun
foreign import CALLCONV "dynamic" mkGetImageInfoFun :: FunPtr GetImageInfoFun -> GetImageInfoFun
foreign import CALLCONV "dynamic" mkCreateSamplerFun :: FunPtr CreateSamplerFun -> CreateSamplerFun
foreign import CALLCONV "dynamic" mkRetainSamplerFun :: FunPtr RetainSamplerFun -> RetainSamplerFun
foreign import CALLCONV "dynamic" mkReleaseSamplerFun :: FunPtr ReleaseSamplerFun -> ReleaseSamplerFun
foreign import CALLCONV "dynamic" mkGetSamplerInfoFun :: FunPtr GetSamplerInfoFun -> GetSamplerInfoFun

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
  let pick a = fmap castFunPtr $ Data.Map.lookup a symPtrs
  let unsafePick a = fromMaybe nullFunPtr $ pick a
  let mandatory a = if (pick a == Nothing) then notFound a else unsafePick a

  let v_1_0 = mandatory
  let v_1_0_gl = unsafePick

  return OpenCLLibrary {
    libHandle = dl,
    raw_clGetPlatformIDs    = mkGetPlatformIDsFun $ v_1_0 "clGetPlatformIDs",
    raw_clGetPlatformInfo   = mkGetPlatformInfoFun $ v_1_0 "clGetPlatformInfo",
    raw_clGetDeviceIDs      = mkGetDeviceIDsFun $ v_1_0 "clGetDeviceIDs",
    raw_clGetDeviceInfo     = mkGetDeviceInfoFun $ v_1_0 "clGetDeviceInfo",
    raw_clCreateBuffer      = mkCreateBufferFun $ v_1_0 "clCreateBuffer",
    raw_clCreateImage2D     = mkCreateImage2DFun $ v_1_0 "clCreateImage2D",
    raw_clCreateImage3D     = mkCreateImage3DFun $ v_1_0 "clCreateImage3D",
    raw_clCreateFromGLTexture2D = mkCreateFromGLTexture2DFun $ v_1_0_gl "clCreateFromGLTexture2D",
    raw_clCreateFromGLBuffer = mkCreateFromGLBufferFun $ v_1_0_gl "clCreateFromGLBuffer",
    raw_clRetainMemObject   = mkRetainMemObjectFun $ v_1_0 "clRetainMemObjectFun",
    raw_clReleaseMemObject  = mkReleaseMemObjectFun $ v_1_0 "clReleaseMemObject",
    raw_clGetSupportedImageFormats = mkGetSupportedImageFormatsFun $ v_1_0 "clGetSupportedImageFormats",
    raw_clGetMemObjectInfo  = mkGetMemObjectInfoFun $ v_1_0 "clGetMemObjectInfo",
    raw_clGetImageInfo      = mkGetImageInfoFun $ v_1_0 "clGetImageInfo",
    raw_clCreateSampler     = mkCreateSamplerFun $ v_1_0 "clCreateSampler",
    raw_clRetainSampler     = mkRetainSamplerFun $ v_1_0 "clRetainSampler",
    raw_clReleaseSampler    = mkReleaseSamplerFun $ v_1_0 "clReleaseSampler",
    raw_clGetSamplerInfo    = mkGetSamplerInfoFun $ v_1_0 "clGetSamplerInfo"
  }

