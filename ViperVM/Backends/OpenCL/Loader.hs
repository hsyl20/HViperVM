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
-- Memory
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
-- Context
type CreateContextFun = Ptr CLContextProperty_ -> CLuint -> Ptr CLDeviceID -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext
type CreateContextFromTypeFun = Ptr CLContextProperty_ -> CLDeviceType_ -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext
type RetainContextFun = CLContext -> IO CLint
type ReleaseContextFun = CLContext -> IO CLint
type GetContextInfoFun = CLContext -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
-- CommandQueue
type CreateCommandQueueFun = CLContext -> CLDeviceID -> CLCommandQueueProperty_ -> Ptr CLint -> IO CLCommandQueue
type RetainCommandQueueFun = CLCommandQueue -> IO CLint
type ReleaseCommandQueueFun = CLCommandQueue -> IO CLint
type GetCommandQueueInfoFun = CLCommandQueue -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
type SetCommandQueuePropertyFun = CLCommandQueue -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO CLint
type EnqueueReadBufferFun = CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueWriteBufferFun = CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueReadImageFun = CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueWriteImageFun = CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueCopyImageFun = CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueCopyImageToBufferFun = CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueCopyBufferToImageFun = CLCommandQueue -> CLMem -> CLMem -> CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueMapBufferFun = CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ())
type EnqueueMapImageFun = CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ())
type EnqueueUnmapMemObjectFun = CLCommandQueue -> CLMem -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueNDRangeKernelFun = CLCommandQueue -> CLKernel -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueNativeKernelFun = CLCommandQueue ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> CLuint -> Ptr CLMem -> Ptr (Ptr ()) -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueTaskFun = CLCommandQueue -> CLKernel -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
type EnqueueMarkerFun = CLCommandQueue -> Ptr CLEvent -> IO CLint 
type EnqueueWaitForEventsFun = CLCommandQueue -> CLuint -> Ptr CLEvent -> IO CLint
type EnqueueBarrierFun = CLCommandQueue -> IO CLint 
type FlushFun = CLCommandQueue -> IO CLint
type FinishFun = CLCommandQueue -> IO CLint



data OpenCLLibrary = OpenCLLibrary {
  libHandle :: DL,
  raw_clGetPlatformIDs    :: GetPlatformIDsFun,
  raw_clGetPlatformInfo   :: GetPlatformInfoFun,
  raw_clGetDeviceIDs      :: GetDeviceIDsFun,
  raw_clGetDeviceInfo     :: GetDeviceInfoFun,
  -- Memory
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
  raw_clGetSamplerInfo    :: GetSamplerInfoFun,
  -- Context
  raw_clCreateContext     :: CreateContextFun,
  raw_clCreateContextFromType :: CreateContextFromTypeFun,
  raw_clRetainContext     :: RetainContextFun,
  raw_clReleaseContext    :: ReleaseContextFun,
  raw_clGetContextInfo    :: GetContextInfoFun,
  -- CommandQueue
  raw_clCreateCommandQueue :: CreateCommandQueueFun,
  raw_clRetainCommandQueue :: RetainCommandQueueFun,
  raw_clReleaseCommandQueue :: ReleaseCommandQueueFun,
  raw_clGetCommandQueueInfo :: GetCommandQueueInfoFun,
  raw_clSetCommandQueueProperty :: SetCommandQueuePropertyFun,
  raw_clEnqueueReadBuffer :: EnqueueReadBufferFun,
  raw_clEnqueueWriteBuffer :: EnqueueWriteBufferFun,
  raw_clEnqueueReadImage :: EnqueueReadImageFun,
  raw_clEnqueueWriteImage :: EnqueueWriteImageFun,
  raw_clEnqueueCopyImage :: EnqueueCopyImageFun,
  raw_clEnqueueCopyImageToBuffer :: EnqueueCopyImageToBufferFun,
  raw_clEnqueueCopyBufferToImage :: EnqueueCopyBufferToImageFun,
  raw_clEnqueueMapBuffer :: EnqueueMapBufferFun,
  raw_clEnqueueMapImage :: EnqueueMapImageFun,
  raw_clEnqueueUnmapMemObject :: EnqueueUnmapMemObjectFun,
  raw_clEnqueueNDRangeKernel :: EnqueueNDRangeKernelFun,
  raw_clEnqueueNativeKernel :: EnqueueNativeKernelFun,
  raw_clEnqueueTask :: EnqueueTaskFun,
  raw_clEnqueueMarker :: EnqueueMarkerFun,
  raw_clEnqueueWaitForEvents :: EnqueueWaitForEventsFun,
  raw_clEnqueueBarrier :: EnqueueBarrierFun,
  raw_clFlush :: FlushFun,
  raw_clFinish :: FinishFun
}

foreign import CALLCONV "dynamic" mkGetPlatformIDsFun :: FunPtr GetPlatformIDsFun -> GetPlatformIDsFun
foreign import CALLCONV "dynamic" mkGetPlatformInfoFun :: FunPtr GetPlatformInfoFun -> GetPlatformInfoFun
foreign import CALLCONV "dynamic" mkGetDeviceIDsFun :: FunPtr GetDeviceIDsFun -> GetDeviceIDsFun
foreign import CALLCONV "dynamic" mkGetDeviceInfoFun :: FunPtr GetDeviceInfoFun -> GetDeviceInfoFun
-- Memory
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
-- Context
foreign import CALLCONV "dynamic" mkCreateContextFun :: FunPtr CreateContextFun -> CreateContextFun
foreign import CALLCONV "dynamic" mkCreateContextFromTypeFun :: FunPtr CreateContextFromTypeFun -> CreateContextFromTypeFun
foreign import CALLCONV "dynamic" mkRetainContextFun :: FunPtr RetainContextFun -> RetainContextFun
foreign import CALLCONV "dynamic" mkReleaseContextFun :: FunPtr ReleaseContextFun -> ReleaseContextFun
foreign import CALLCONV "dynamic" mkGetContextInfoFun :: FunPtr GetContextInfoFun -> GetContextInfoFun
-- CommandQueue
foreign import CALLCONV "dynamic" mkCreateCommandQueueFun :: FunPtr CreateCommandQueueFun -> CreateCommandQueueFun
foreign import CALLCONV "dynamic" mkRetainCommandQueueFun :: FunPtr RetainCommandQueueFun -> RetainCommandQueueFun
foreign import CALLCONV "dynamic" mkReleaseCommandQueueFun :: FunPtr ReleaseCommandQueueFun -> ReleaseCommandQueueFun
foreign import CALLCONV "dynamic" mkGetCommandQueueInfoFun :: FunPtr GetCommandQueueInfoFun -> GetCommandQueueInfoFun
foreign import CALLCONV "dynamic" mkSetCommandQueuePropertyFun :: FunPtr SetCommandQueuePropertyFun -> SetCommandQueuePropertyFun
foreign import CALLCONV "dynamic" mkEnqueueReadBufferFun :: FunPtr EnqueueReadBufferFun -> EnqueueReadBufferFun
foreign import CALLCONV "dynamic" mkEnqueueWriteBufferFun :: FunPtr EnqueueWriteBufferFun -> EnqueueWriteBufferFun
foreign import CALLCONV "dynamic" mkEnqueueReadImageFun :: FunPtr EnqueueReadImageFun -> EnqueueReadImageFun
foreign import CALLCONV "dynamic" mkEnqueueWriteImageFun :: FunPtr EnqueueWriteImageFun -> EnqueueWriteImageFun
foreign import CALLCONV "dynamic" mkEnqueueCopyImageFun :: FunPtr EnqueueCopyImageFun -> EnqueueCopyImageFun
foreign import CALLCONV "dynamic" mkEnqueueCopyImageToBufferFun :: FunPtr EnqueueCopyImageToBufferFun -> EnqueueCopyImageToBufferFun
foreign import CALLCONV "dynamic" mkEnqueueCopyBufferToImageFun :: FunPtr EnqueueCopyBufferToImageFun -> EnqueueCopyBufferToImageFun
foreign import CALLCONV "dynamic" mkEnqueueMapBufferFun :: FunPtr EnqueueMapBufferFun -> EnqueueMapBufferFun
foreign import CALLCONV "dynamic" mkEnqueueMapImageFun :: FunPtr EnqueueMapImageFun -> EnqueueMapImageFun
foreign import CALLCONV "dynamic" mkEnqueueUnmapMemObjectFun :: FunPtr EnqueueUnmapMemObjectFun -> EnqueueUnmapMemObjectFun
foreign import CALLCONV "dynamic" mkEnqueueNDRangeKernelFun :: FunPtr EnqueueNDRangeKernelFun -> EnqueueNDRangeKernelFun
foreign import CALLCONV "dynamic" mkEnqueueNativeKernelFun :: FunPtr EnqueueNativeKernelFun -> EnqueueNativeKernelFun
foreign import CALLCONV "dynamic" mkEnqueueTaskFun :: FunPtr EnqueueTaskFun -> EnqueueTaskFun
foreign import CALLCONV "dynamic" mkEnqueueMarkerFun :: FunPtr EnqueueMarkerFun -> EnqueueMarkerFun
foreign import CALLCONV "dynamic" mkEnqueueWaitForEventsFun :: FunPtr EnqueueWaitForEventsFun -> EnqueueWaitForEventsFun
foreign import CALLCONV "dynamic" mkEnqueueBarrierFun :: FunPtr EnqueueBarrierFun -> EnqueueBarrierFun
foreign import CALLCONV "dynamic" mkFlushFun :: FunPtr FlushFun -> FlushFun
foreign import CALLCONV "dynamic" mkFinishFun :: FunPtr FinishFun -> FinishFun

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
    -- Memory
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
    raw_clGetSamplerInfo    = mkGetSamplerInfoFun $ v_1_0 "clGetSamplerInfo",
    -- Context
    raw_clCreateContext     = mkCreateContextFun $ v_1_0 "clCreateContext",
    raw_clCreateContextFromType = mkCreateContextFromTypeFun $ v_1_0 "clCreateContextFromType",
    raw_clRetainContext     = mkRetainContextFun $ v_1_0 "clRetainContext",
    raw_clReleaseContext    = mkReleaseContextFun $ v_1_0 "clReleaseContext",
    raw_clGetContextInfo    = mkGetContextInfoFun $ v_1_0 "clGetContextInfo",
    -- CommandQueue
    raw_clCreateCommandQueue  = mkCreateCommandQueueFun $ v_1_0 "clCreateCommandQueue",
    raw_clRetainCommandQueue  = mkRetainCommandQueueFun $ v_1_0 "clRetainCommandQueue",
    raw_clReleaseCommandQueue = mkReleaseCommandQueueFun $ v_1_0 "clReleaseCommandQueue",
    raw_clGetCommandQueueInfo = mkGetCommandQueueInfoFun $ v_1_0 "clGetCommandQueueInfo",
    raw_clSetCommandQueueProperty = mkSetCommandQueuePropertyFun $ v_1_0 "clSetCommandQueueProperty",
    raw_clEnqueueReadBuffer   = mkEnqueueReadBufferFun $ v_1_0 "clEnqueueReadBuffer",
    raw_clEnqueueWriteBuffer  = mkEnqueueWriteBufferFun $ v_1_0 "clEnqueueWriteBuffer",
    raw_clEnqueueReadImage    = mkEnqueueReadImageFun $ v_1_0 "clEnqueueReadImage",
    raw_clEnqueueWriteImage   = mkEnqueueWriteImageFun $ v_1_0 "clEnqueueWriteImage",
    raw_clEnqueueCopyImage    = mkEnqueueCopyImageFun $ v_1_0 "clEnqueueCopyImage",
    raw_clEnqueueCopyImageToBuffer = mkEnqueueCopyImageToBufferFun $ v_1_0 "clEnqueueCopyImageToBuffer",
    raw_clEnqueueCopyBufferToImage = mkEnqueueCopyBufferToImageFun $ v_1_0 "clEnqueueCopyBufferToImage",
    raw_clEnqueueMapBuffer    = mkEnqueueMapBufferFun $ v_1_0 "clEnqueueMapBuffer",
    raw_clEnqueueMapImage     = mkEnqueueMapImageFun $ v_1_0 "clEnqueueMapImage",
    raw_clEnqueueUnmapMemObject = mkEnqueueUnmapMemObjectFun $ v_1_0 "clEnqueueUnmapMemObject",
    raw_clEnqueueNDRangeKernel = mkEnqueueNDRangeKernelFun $ v_1_0 "clEnqueueNDRangeKernel",
    raw_clEnqueueNativeKernel = mkEnqueueNativeKernelFun $ v_1_0 "clEnqueueNativeKernel",
    raw_clEnqueueTask         = mkEnqueueTaskFun $ v_1_0 "clEnqueueTask",
    raw_clEnqueueMarker       = mkEnqueueMarkerFun $ v_1_0 "clEnqueueMarker",
    raw_clEnqueueWaitForEvents = mkEnqueueWaitForEventsFun $ v_1_0 "clEnqueueWaitForEvents",
    raw_clEnqueueBarrier      = mkEnqueueBarrierFun $ v_1_0 "clEnqueueBarrier",
    raw_clFlush               = mkFlushFun $ v_1_0 "clFlush",
    raw_clFinish              = mkFinishFun $ v_1_0 "clFinish"
  }

