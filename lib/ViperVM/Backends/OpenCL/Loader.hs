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
import Control.Applicative

-- | OpenCL Library module object
data OpenCLLibrary = OpenCLLibrary {
  libHandle :: DL,
  rawClGetPlatformIDs    :: CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint,
  rawClGetPlatformInfo   :: CLPlatformID -> CLPlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClGetDeviceIDs      :: CLPlatformID -> CLDeviceType_ -> CLuint -> Ptr CLDeviceID -> Ptr CLuint -> IO CLint,
  rawClGetDeviceInfo     :: CLDeviceID -> CLDeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,

  -- Memory
  rawClCreateBuffer      :: CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  rawClCreateImage2D     :: CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  rawClCreateImage3D     :: CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> Ptr CLint -> IO CLMem,
  rawClCreateFromGLTexture2D :: CLContext -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> Ptr CLint -> IO CLMem,
  rawClCreateFromGLBuffer :: CLContext -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO CLMem,
  rawClRetainMemObject   :: CLMem -> IO CLint,
  rawClReleaseMemObject  :: CLMem -> IO CLint,
  rawClGetSupportedImageFormats :: CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p -> Ptr CLuint -> IO CLint,
  rawClGetMemObjectInfo  :: CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClGetImageInfo      :: CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClCreateSampler     :: CLContext -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO CLSampler,
  rawClRetainSampler     :: CLSampler -> IO CLint,
  rawClReleaseSampler    :: CLSampler -> IO CLint,
  rawClGetSamplerInfo    :: CLSampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,

  -- Context
  rawClCreateContext     :: Ptr CLContextProperty_ -> CLuint -> Ptr CLDeviceID -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext,
  rawClCreateContextFromType :: Ptr CLContextProperty_ -> CLDeviceType_ -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO CLContext,
  rawClRetainContext     :: CLContext -> IO CLint,
  rawClReleaseContext    :: CLContext -> IO CLint,
  rawClGetContextInfo    :: CLContext -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,

  -- CommandQueue
  rawClCreateCommandQueue    :: CLContext -> CLDeviceID -> CLCommandQueueProperty_ -> Ptr CLint -> IO CLCommandQueue,
  rawClRetainCommandQueue    :: CLCommandQueue -> IO CLint,
  rawClReleaseCommandQueue   :: CLCommandQueue -> IO CLint,
  rawClGetCommandQueueInfo   :: CLCommandQueue -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClSetCommandQueueProperty :: CLCommandQueue -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO CLint,
  rawClEnqueueReadBuffer     :: CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueReadBufferRect :: Maybe(CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint),
  rawClEnqueueWriteBuffer    :: CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueWriteBufferRect :: Maybe(CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint),
  rawClEnqueueCopyBuffer    :: CLCommandQueue -> CLMem -> CLMem -> CSize -> CSize ->  CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueCopyBufferRect :: Maybe(CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint),
  rawClEnqueueReadImage      :: CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueWriteImage     :: CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueCopyImage      :: CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueCopyImageToBuffer :: CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueCopyBufferToImage :: CLCommandQueue -> CLMem -> CLMem -> CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueMapBuffer      :: CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ()),
  rawClEnqueueMapImage       :: CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ()),
  rawClEnqueueUnmapMemObject :: CLCommandQueue -> CLMem -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueNDRangeKernel  :: CLCommandQueue -> CLKernel -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueNativeKernel   :: CLCommandQueue ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> CLuint -> Ptr CLMem -> Ptr (Ptr ()) -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueTask           :: CLCommandQueue -> CLKernel -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint,
  rawClEnqueueMarker         :: CLCommandQueue -> Ptr CLEvent -> IO CLint ,
  rawClEnqueueWaitForEvents  :: CLCommandQueue -> CLuint -> Ptr CLEvent -> IO CLint,
  rawClEnqueueBarrier        :: CLCommandQueue -> IO CLint ,
  rawClFlush                 :: CLCommandQueue -> IO CLint,
  rawClFinish                :: CLCommandQueue -> IO CLint,
  -- Event
  rawClWaitForEvents         :: CLuint -> Ptr CLEvent -> IO CLint,
  rawClGetEventInfo          :: CLEvent -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClRetainEvent           :: CLEvent -> IO CLint ,
  rawClReleaseEvent          :: CLEvent -> IO CLint ,
  rawClGetEventProfilingInfo :: CLEvent -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  -- Program
  rawClCreateProgramWithSource :: CLContext -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO CLProgram,
  rawClCreateProgramWithBinary :: CLContext -> CLuint -> Ptr CLDeviceID -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO CLProgram,
  rawClRetainProgram           :: CLProgram -> IO CLint,
  rawClReleaseProgram          :: CLProgram -> IO CLint,
  rawClBuildProgram            :: CLProgram -> CLuint -> Ptr CLDeviceID -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint,
  rawClUnloadCompiler          :: IO CLint,
  rawClGetProgramInfo          :: CLProgram -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClGetProgramBuildInfo     :: CLProgram -> CLDeviceID -> CLProgramBuildInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClCreateKernel            :: CLProgram -> CString -> Ptr CLint -> IO CLKernel ,
  rawClCreateKernelsInProgram  :: CLProgram -> CLuint -> Ptr CLKernel -> Ptr CLuint -> IO CLint ,
  rawClRetainKernel            :: CLKernel -> IO CLint ,
  rawClReleaseKernel           :: CLKernel -> IO CLint ,
  rawClSetKernelArg            :: CLKernel -> CLuint -> CSize -> Ptr () -> IO CLint,
  rawClGetKernelInfo           :: CLKernel -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint,
  rawClGetKernelWorkGroupInfo  :: CLKernel -> CLDeviceID -> CLKernelWorkGroupInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
}

instance Eq OpenCLLibrary where
  (==) a b = (==) (packDL $ libHandle a) (packDL $ libHandle b)

instance Ord OpenCLLibrary where
  compare a b = compare (packDL $ libHandle a) (packDL $ libHandle b)

myMod :: String -> String
myMod x = fromJust $ ("c" ++) <$> stripPrefix "rawC" x

$(makeDynamicLinker ''OpenCLLibrary CallConv 'myMod)

loadOpenCL :: String -> IO OpenCLLibrary
loadOpenCL lib = loadOpenCLLibrary lib [RTLD_NOW,RTLD_LOCAL]
