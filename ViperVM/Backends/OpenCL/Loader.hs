{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, CPP #-}

module ViperVM.Backends.OpenCL.Loader (
  OpenCLLibrary(..),
  loadOpenCL
) where

import System.Posix.DynamicLinker.Template
import System.Posix.DynamicLinker

import ViperVM.Backends.OpenCL.Types
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Maybe
import Data.List (stripPrefix)

-- | OpenCL Library module object
data OpenCLLibrary = OpenCLLibrary {
  libHandle :: DL,
  raw_clGetPlatformIDs    :: CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint,
  raw_clGetPlatformInfo   :: CLPlatformID -> CLPlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clGetDeviceIDs      :: CLPlatformID -> CLDeviceType_ -> CLuint -> Ptr CLDeviceID -> Ptr CLuint -> IO CLint,
  raw_clGetDeviceInfo     :: CLDeviceID -> CLDeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  -- Memory
  raw_clCreateBuffer      :: CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  raw_clCreateImage2D     :: CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  raw_clCreateImage3D     :: CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  raw_clCreateFromGLTexture2D :: CLContext -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> Ptr CLint -> IO CLMem,
  raw_clCreateFromGLBuffer :: CLContext -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO CLMem,
  raw_clRetainMemObject   :: CLMem -> IO CLint,
  raw_clReleaseMemObject  :: CLMem -> IO CLint,
  raw_clGetSupportedImageFormats :: CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p -> Ptr CLuint -> IO CLint,
  raw_clGetMemObjectInfo  :: CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clGetImageInfo      :: CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clCreateSampler     :: CLContext -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO CLSampler,
  raw_clRetainSampler     :: CLSampler -> IO CLint,
  raw_clReleaseSampler    :: CLSampler -> IO CLint,
  raw_clGetSamplerInfo    :: CLSampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  -- Context
  raw_clCreateContext     :: Ptr CLContextProperty_ -> CLuint -> Ptr CLDeviceID -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext,
  raw_clCreateContextFromType :: Ptr CLContextProperty_ -> CLDeviceType_ -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext,
  raw_clRetainContext     :: CLContext -> IO CLint,
  raw_clReleaseContext    :: CLContext -> IO CLint,
  raw_clGetContextInfo    :: CLContext -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  -- CommandQueue
  raw_clCreateCommandQueue    :: CLContext -> CLDeviceID -> CLCommandQueueProperty_ -> Ptr CLint -> IO CLCommandQueue,
  raw_clRetainCommandQueue    :: CLCommandQueue -> IO CLint,
  raw_clReleaseCommandQueue   :: CLCommandQueue -> IO CLint,
  raw_clGetCommandQueueInfo   :: CLCommandQueue -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clSetCommandQueueProperty :: CLCommandQueue -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO CLint,
  raw_clEnqueueReadBuffer     :: CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueWriteBuffer    :: CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueReadImage      :: CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueWriteImage     :: CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueCopyImage      :: CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueCopyImageToBuffer :: CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueCopyBufferToImage :: CLCommandQueue -> CLMem -> CLMem -> CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueMapBuffer      :: CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ()),
  raw_clEnqueueMapImage       :: CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ()),
  raw_clEnqueueUnmapMemObject :: CLCommandQueue -> CLMem -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueNDRangeKernel  :: CLCommandQueue -> CLKernel -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueNativeKernel   :: CLCommandQueue ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> CLuint -> Ptr CLMem -> Ptr (Ptr ()) -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueTask           :: CLCommandQueue -> CLKernel -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueMarker         :: CLCommandQueue -> Ptr CLEvent -> IO CLint ,
  raw_clEnqueueWaitForEvents  :: CLCommandQueue -> CLuint -> Ptr CLEvent -> IO CLint,
  raw_clEnqueueBarrier        :: CLCommandQueue -> IO CLint ,
  raw_clFlush                 :: CLCommandQueue -> IO CLint,
  raw_clFinish                :: CLCommandQueue -> IO CLint,
  -- Event
  raw_clWaitForEvents         :: CLuint -> Ptr CLEvent -> IO CLint,
  raw_clGetEventInfo          :: CLEvent -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clRetainEvent           :: CLEvent -> IO CLint ,
  raw_clReleaseEvent          :: CLEvent -> IO CLint ,
  raw_clGetEventProfilingInfo :: CLEvent -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  -- Program
  raw_clCreateProgramWithSource :: CLContext -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO CLProgram,
  raw_clCreateProgramWithBinary :: CLContext -> CLuint -> Ptr CLDeviceID -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO CLProgram,
  raw_clRetainProgram           :: CLProgram -> IO CLint,
  raw_clReleaseProgram          :: CLProgram -> IO CLint,
  raw_clBuildProgram            :: CLProgram -> CLuint -> Ptr CLDeviceID -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint,
  raw_clUnloadCompiler          :: IO CLint,
  raw_clGetProgramInfo          :: CLProgram -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clGetProgramBuildInfo     :: CLProgram -> CLDeviceID -> CLProgramBuildInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clCreateKernel            :: CLProgram -> CString -> Ptr CLint -> IO CLKernel ,
  raw_clCreateKernelsInProgram  :: CLProgram -> CLuint -> Ptr CLKernel -> Ptr CLuint -> IO CLint ,
  raw_clRetainKernel            :: CLKernel -> IO CLint ,
  raw_clReleaseKernel           :: CLKernel -> IO CLint ,
  raw_clSetKernelArg            :: CLKernel -> CLuint -> CSize -> Ptr () -> IO CLint,
  raw_clGetKernelInfo           :: CLKernel -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  raw_clGetKernelWorkGroupInfo  :: CLKernel -> CLDeviceID -> CLKernelWorkGroupInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
}

instance Eq OpenCLLibrary where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord OpenCLLibrary where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)

myMod :: String -> String
myMod = fromJust . stripPrefix "raw_"

$(makeDynamicLinker ''OpenCLLibrary CCall 'myMod)

loadOpenCL :: String -> IO OpenCLLibrary
loadOpenCL lib = loadOpenCLLibrary lib [RTLD_NOW,RTLD_LOCAL]
