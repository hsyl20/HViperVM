{- Copyright (c) 2011 Luis Cabellos,

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of  nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, CPP #-}
module ViperVM.Backends.OpenCL.Query( 
  -- * Types
  CLPlatformInfo(..), CLPlatformID, CLDeviceID, CLDeviceType(..),
  CLDeviceFPConfig(..), CLDeviceExecCapability(..), CLDeviceLocalMemType(..),
  CLDeviceMemCacheType(..),
  -- * Platform Query Functions
  clGetPlatformIDs, clGetPlatformInfo, 
  -- * Device Query Functions
  clGetDeviceIDs, clGetDeviceExecutionCapabilities,
  clGetDeviceAddressBits, clGetDeviceAvailable, clGetDeviceCompilerAvailable, 
  clGetDeviceEndianLittle, clGetDeviceErrorCorrectionSupport, 
  clGetDeviceExtensions, clGetDeviceGlobalMemCacheSize, 
  clGetDeviceGlobalMemCachelineSize, clGetDeviceGlobalMemSize, 
  clGetDeviceImageSupport, clGetDeviceImage2DMaxHeight, 
  clGetDeviceImage2DMaxWidth, clGetDeviceImage3DMaxDepth, 
  clGetDeviceImage3DMaxHeight, clGetDeviceImage3DMaxWidth, 
  clGetDeviceLocalMemSize, clGetDeviceMaxClockFrequency, 
  clGetDeviceMaxComputeUnits, clGetDeviceMaxConstantArgs, 
  clGetDeviceMaxConstantBufferSize, clGetDeviceMaxMemAllocSize, 
  clGetDeviceMaxParameterSize, clGetDeviceMaxReadImageArgs, 
  clGetDeviceMaxSamplers, clGetDeviceMaxWorkGroupSize, 
  clGetDeviceMaxWorkItemDimensions, clGetDeviceMaxWorkItemSizes, 
  clGetDeviceMaxWriteImageArgs, clGetDeviceMemBaseAddrAlign, 
  clGetDeviceMinDataTypeAlignSize, clGetDeviceName, clGetDevicePlatform, 
  clGetDevicePreferredVectorWidthChar, clGetDevicePreferredVectorWidthShort, 
  clGetDevicePreferredVectorWidthInt, clGetDevicePreferredVectorWidthLong, 
  clGetDevicePreferredVectorWidthFloat, clGetDevicePreferredVectorWidthDouble, 
  clGetDeviceProfile, clGetDeviceProfilingTimerResolution, clGetDeviceVendor, 
  clGetDeviceVendorID, clGetDeviceVersion, clGetDeviceDriverVersion, 
  clGetDeviceSingleFPConfig, clGetDeviceDoubleFPConfig, 
  clGetDeviceHalfFPConfig, clGetDeviceLocalMemType, 
  clGetDeviceGlobalMemCacheType, clGetDeviceQueueProperties, clGetDeviceType )
       where

-- -----------------------------------------------------------------------------
import Foreign( Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray )
import Foreign.C.String( CString, peekCString )
import Foreign.C.Types( CSize )
import Foreign.Storable( sizeOf )
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Types( 
  CLbool, CLuint, CLulong, CLDeviceType_,
  CLDeviceInfo_, CLDeviceFPConfig(..), CLDeviceExecCapability(..),
  CLDeviceLocalMemType(..), CLDeviceMemCacheType(..), CLPlatformInfo(..),
  CLPlatformID, CLDeviceID, CLDeviceType(..), CLCommandQueueProperty, 
  getCLValue, getEnumCL, bitmaskToDeviceTypes, bitmaskToCommandQueueProperties, 
  whenSuccess, bitmaskToFPConfig, bitmaskToExecCapability )

-- -----------------------------------------------------------------------------

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
getNumPlatforms :: OpenCLLibrary -> IO CLuint
getNumPlatforms lib = alloca $ \(num_platforms :: Ptr CLuint) -> do 
  whenSuccess (raw_clGetPlatformIDs lib 0 nullPtr num_platforms) 
    $ peek num_platforms

-- | Obtain the list of platforms available. Returns the list if the function 
-- is executed successfully. Otherwise it returns the empty list.
clGetPlatformIDs :: OpenCLLibrary -> IO [CLPlatformID]
clGetPlatformIDs lib = do
  nplats <- getNumPlatforms lib
  allocaArray (fromIntegral nplats) $ \(plats :: Ptr CLPlatformID) -> do
    whenSuccess (raw_clGetPlatformIDs lib nplats plats nullPtr) 
      $ peekArray (fromIntegral nplats) plats
  
getPlatformInfoSize :: OpenCLLibrary -> CLPlatformID -> CLuint -> IO CSize
getPlatformInfoSize lib platform infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (raw_clGetPlatformInfo lib platform infoid 0 nullPtr value_size) 
    $ peek value_size
  
-- | Get specific information about the OpenCL platform. It returns Nothing if
-- platform is not a valid platform.
clGetPlatformInfo :: OpenCLLibrary -> CLPlatformInfo -> CLPlatformID -> IO String
clGetPlatformInfo lib infoid platform = do
  sval <- getPlatformInfoSize lib platform infocl
  allocaArray (fromIntegral sval) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetPlatformInfo lib platform infocl sval (castPtr buff) nullPtr)
      $ peekCString buff
    where
      infocl = getCLValue infoid

-- -----------------------------------------------------------------------------
getNumDevices :: OpenCLLibrary -> CLPlatformID -> CLDeviceType_ -> IO CLuint
getNumDevices lib platform dtype = alloca $ \(num_devices :: Ptr CLuint) -> do
  whenSuccess (raw_clGetDeviceIDs lib platform dtype 0 nullPtr num_devices)
    $ peek num_devices

-- | Obtain the list of devices available on a platform. Returns the list if 
-- the function is executed successfully. Otherwise it returns the empty list 
-- if platform is not a valid platform or no OpenCL devices that matched 
-- device_type were found.
clGetDeviceIDs :: OpenCLLibrary -> CLDeviceType -> CLPlatformID -> IO [CLDeviceID]
clGetDeviceIDs lib dtype platform = do
  ndevs <- getNumDevices lib platform dval
  allocaArray (fromIntegral ndevs) $ \(devs :: Ptr CLDeviceID) -> do
    whenSuccess (raw_clGetDeviceIDs lib platform dval ndevs devs nullPtr)
      $ peekArray (fromIntegral ndevs) devs
    where
      dval = getCLValue dtype

getDeviceInfoSize :: OpenCLLibrary -> CLDeviceID -> CLDeviceInfo_ -> IO CSize
getDeviceInfoSize lib device infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid 0 nullPtr value_size)
    $ peek value_size
  
getDeviceInfoString :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO String
getDeviceInfoString lib infoid device = do
  n <- getDeviceInfoSize lib device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetDeviceInfo lib device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
  
getDeviceInfoUint :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CLuint
getDeviceInfoUint lib infoid device = alloca $ \(dat :: Ptr CLuint) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CLuint)

getDeviceInfoUlong :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CLulong
getDeviceInfoUlong lib infoid device = alloca $ \(dat :: Ptr CLulong) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CLulong)

getDeviceInfoSizet :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CSize
getDeviceInfoSizet lib infoid device = alloca $ \(dat :: Ptr CSize) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CSize)
  
getDeviceInfoBool :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO Bool
getDeviceInfoBool lib infoid device = alloca $ \(dat :: Ptr CLbool) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat >>= return . (/=0)
    where 
      size = fromIntegral $ sizeOf (0::CLbool)
  
getDeviceInfoFP :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO [CLDeviceFPConfig]
getDeviceInfoFP lib infoid device = do
  flags <- getDeviceInfoUlong lib infoid device
  return . bitmaskToFPConfig $ flags

#c
enum CLDeviceInfo {
  cL_DEVICE_ADDRESS_BITS=CL_DEVICE_ADDRESS_BITS,
  cL_DEVICE_AVAILABLE=CL_DEVICE_AVAILABLE,
  cL_DEVICE_COMPILER_AVAILABLE=CL_DEVICE_COMPILER_AVAILABLE,
#ifdef CL_DEVICE_DOUBLE_FP_CONFIG  
  cL_DEVICE_DOUBLE_FP_CONFIG=CL_DEVICE_DOUBLE_FP_CONFIG,
#else
  cL_DEVICE_DOUBLE_FP_CONFIG=0x1032,
#endif
  cL_DEVICE_ENDIAN_LITTLE=CL_DEVICE_ENDIAN_LITTLE,
  cL_DEVICE_ERROR_CORRECTION_SUPPORT=CL_DEVICE_ERROR_CORRECTION_SUPPORT,
  cL_DEVICE_EXECUTION_CAPABILITIES=CL_DEVICE_EXECUTION_CAPABILITIES,
  cL_DEVICE_EXTENSIONS=CL_DEVICE_EXTENSIONS,
  cL_DEVICE_GLOBAL_MEM_CACHE_SIZE=CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
  cL_DEVICE_GLOBAL_MEM_CACHE_TYPE=CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
  cL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE=CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
  cL_DEVICE_GLOBAL_MEM_SIZE=CL_DEVICE_GLOBAL_MEM_SIZE,
#ifdef CL_DEVICE_HALF_FP_CONFIG
  cL_DEVICE_HALF_FP_CONFIG=CL_DEVICE_HALF_FP_CONFIG,
#else
  cL_DEVICE_HALF_FP_CONFIG=0x1033,
#endif
  cL_DEVICE_IMAGE_SUPPORT=CL_DEVICE_IMAGE_SUPPORT,
  cL_DEVICE_IMAGE2D_MAX_HEIGHT=CL_DEVICE_IMAGE2D_MAX_HEIGHT,
  cL_DEVICE_IMAGE2D_MAX_WIDTH=CL_DEVICE_IMAGE2D_MAX_WIDTH,
  cL_DEVICE_IMAGE3D_MAX_DEPTH=CL_DEVICE_IMAGE3D_MAX_DEPTH,
  cL_DEVICE_IMAGE3D_MAX_HEIGHT=CL_DEVICE_IMAGE3D_MAX_HEIGHT,
  cL_DEVICE_IMAGE3D_MAX_WIDTH=CL_DEVICE_IMAGE3D_MAX_WIDTH,
  cL_DEVICE_LOCAL_MEM_SIZE=CL_DEVICE_LOCAL_MEM_SIZE,
  cL_DEVICE_LOCAL_MEM_TYPE=CL_DEVICE_LOCAL_MEM_TYPE,
  cL_DEVICE_MAX_CLOCK_FREQUENCY=CL_DEVICE_MAX_CLOCK_FREQUENCY,
  cL_DEVICE_MAX_COMPUTE_UNITS=CL_DEVICE_MAX_COMPUTE_UNITS,
  cL_DEVICE_MAX_CONSTANT_ARGS=CL_DEVICE_MAX_CONSTANT_ARGS,
  cL_DEVICE_MAX_CONSTANT_BUFFER_SIZE=CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
  cL_DEVICE_MAX_MEM_ALLOC_SIZE=CL_DEVICE_MAX_MEM_ALLOC_SIZE,
  cL_DEVICE_MAX_PARAMETER_SIZE=CL_DEVICE_MAX_PARAMETER_SIZE,
  cL_DEVICE_MAX_READ_IMAGE_ARGS=CL_DEVICE_MAX_READ_IMAGE_ARGS,
  cL_DEVICE_MAX_SAMPLERS=CL_DEVICE_MAX_SAMPLERS,
  cL_DEVICE_MAX_WORK_GROUP_SIZE=CL_DEVICE_MAX_WORK_GROUP_SIZE,
  cL_DEVICE_MAX_WORK_ITEM_DIMENSIONS=CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
  cL_DEVICE_MAX_WORK_ITEM_SIZES=CL_DEVICE_MAX_WORK_ITEM_SIZES,
  cL_DEVICE_MAX_WRITE_IMAGE_ARGS=CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
  cL_DEVICE_MEM_BASE_ADDR_ALIGN=CL_DEVICE_MEM_BASE_ADDR_ALIGN,
  cL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE=CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
  cL_DEVICE_NAME=CL_DEVICE_NAME,
  cL_DEVICE_PLATFORM=CL_DEVICE_PLATFORM,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR=CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_INT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG=CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE=CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
  cL_DEVICE_PROFILE=CL_DEVICE_PROFILE,
  cL_DEVICE_PROFILING_TIMER_RESOLUTION=CL_DEVICE_PROFILING_TIMER_RESOLUTION,
  cL_DEVICE_QUEUE_PROPERTIES=CL_DEVICE_QUEUE_PROPERTIES,
  cL_DEVICE_SINGLE_FP_CONFIG=CL_DEVICE_SINGLE_FP_CONFIG,
  cL_DEVICE_TYPE=CL_DEVICE_TYPE,
  cL_DEVICE_VENDOR=CL_DEVICE_VENDOR,
  cL_DEVICE_VENDOR_ID=CL_DEVICE_VENDOR_ID,
  cL_DEVICE_VERSION=CL_DEVICE_VERSION,
  cL_DRIVER_VERSION=CL_DRIVER_VERSION,
  };
#endc
{#enum CLDeviceInfo {upcaseFirstLetter} #}

-- | The default compute device address space size specified as an unsigned 
-- integer value in bits. Currently supported values are 32 or 64 bits.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_ADDRESS_BITS'.
clGetDeviceAddressBits :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceAddressBits lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_ADDRESS_BITS

-- | Is 'True' if the device is available and 'False' if the device is not 
-- available.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_AVAILABLE'.
clGetDeviceAvailable :: OpenCLLibrary -> CLDeviceID -> IO Bool
clGetDeviceAvailable lib = (getDeviceInfoBool lib) . getCLValue $ CL_DEVICE_AVAILABLE

-- | Is 'False' if the implementation does not have a compiler available to 
-- compile the program source. Is 'True' if the compiler is available. This can 
-- be 'False' for the embededed platform profile only.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_COMPILER_AVAILABLE'.
clGetDeviceCompilerAvailable :: OpenCLLibrary -> CLDeviceID -> IO Bool
clGetDeviceCompilerAvailable lib = (getDeviceInfoBool lib) . getCLValue $ CL_DEVICE_COMPILER_AVAILABLE

-- | Describes the OPTIONAL double precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The mandated minimum double precision floating-point capability is 
-- 'CL_FP_FMA' | 'CL_FP_ROUND_TO_NEAREST' | 'CL_FP_ROUND_TO_ZERO' | 
-- 'CL_FP_ROUND_TO_INF' | 'CL_FP_INF_NAN' | 'CL_FP_DENORM'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_DOUBLE_FP_CONFIG'.
clGetDeviceDoubleFPConfig :: OpenCLLibrary -> CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceDoubleFPConfig lib = (getDeviceInfoFP lib) . getCLValue $ CL_DEVICE_DOUBLE_FP_CONFIG

-- | Is 'True' if the OpenCL device is a little endian device and 'False' 
-- otherwise.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_ENDIAN_LITTLE'.
clGetDeviceEndianLittle :: OpenCLLibrary -> CLDeviceID -> IO Bool
clGetDeviceEndianLittle lib = (getDeviceInfoBool lib) . getCLValue $ CL_DEVICE_ENDIAN_LITTLE

-- | Is 'True' if the device implements error correction for the memories, 
-- caches, registers etc. in the device. Is 'False' if the device does not 
-- implement error correction. This can be a requirement for certain clients of 
-- OpenCL.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_ERROR_CORRECTION_SUPPORT'.
clGetDeviceErrorCorrectionSupport :: OpenCLLibrary -> CLDeviceID -> IO Bool
clGetDeviceErrorCorrectionSupport lib = (getDeviceInfoBool lib) . getCLValue $ CL_DEVICE_ERROR_CORRECTION_SUPPORT

-- | Describes the execution capabilities of the device. This is a list that 
-- describes one or more of the 'CLDeviceExecCapability' values.
-- The mandated minimum capability is 'CL_EXEC_KERNEL'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_EXECUTION_CAPABILITIES'.
clGetDeviceExecutionCapabilities :: OpenCLLibrary -> CLDeviceID -> IO [CLDeviceExecCapability]
clGetDeviceExecutionCapabilities lib device = do
  flags <- getDeviceInfoUlong lib (getCLValue CL_DEVICE_EXECUTION_CAPABILITIES) device
  return . bitmaskToExecCapability $ flags

-- | Returns a space separated list of extension names (the extension names 
-- themselves do not contain any spaces). The list of extension names returned 
-- currently can include one or more of the following approved extension names:
--
-- * cl_khr_fp64
--
-- * cl_khr_select_fprounding_mode
--
-- * cl_khr_global_int32_base_atomics
--
-- * cl_khr_global_int32_extended_atomics
--
-- * cl_khr_local_int32_base_atomics
--
-- * cl_khr_local_int32_extended_atomics
--
-- * cl_khr_int64_base_atomics
--
-- * cl_khr_int64_extended_atomics
--
-- * cl_khr_3d_image_writes
--
-- * cl_khr_byte_addressable_store
--
-- * cl_khr_fp16
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_EXTENSIONS'.
clGetDeviceExtensions :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceExtensions lib = (getDeviceInfoString lib) . getCLValue $ CL_DEVICE_EXTENSIONS

-- | Size of global memory cache in bytes.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_GLOBAL_MEM_CACHE_SIZE'.
clGetDeviceGlobalMemCacheSize :: OpenCLLibrary -> CLDeviceID -> IO CLulong
clGetDeviceGlobalMemCacheSize lib = (getDeviceInfoUlong lib) . getCLValue $ CL_DEVICE_GLOBAL_MEM_CACHE_SIZE

-- | Type of global memory cache supported. Valid values are: 'CL_NONE', 
-- 'CL_READ_ONLY_CACHE', and 'CL_READ_WRITE_CACHE'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_GLOBAL_MEM_CACHE_TYPE'.
clGetDeviceGlobalMemCacheType :: OpenCLLibrary -> CLDeviceID -> IO CLDeviceMemCacheType
clGetDeviceGlobalMemCacheType lib device = do
  typ <- getDeviceInfoUint lib (getCLValue CL_DEVICE_GLOBAL_MEM_CACHE_TYPE) device 
  return . getEnumCL $ typ

-- | Size of global memory cache line in bytes.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE'.
clGetDeviceGlobalMemCachelineSize :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceGlobalMemCachelineSize lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE

-- | Size of global device memory in bytes.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_GLOBAL_MEM_SIZE'.
clGetDeviceGlobalMemSize :: OpenCLLibrary -> CLDeviceID -> IO CLulong
clGetDeviceGlobalMemSize lib = (getDeviceInfoUlong lib) . getCLValue $ CL_DEVICE_GLOBAL_MEM_SIZE

-- | Describes the OPTIONAL half precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The required minimum half precision floating-point capability as implemented 
-- by this extension is 'CL_FP_ROUND_TO_ZERO' | 'CL_FP_ROUND_TO_INF' | 
-- 'CL_FP_INF_NAN'.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_HALF_FP_CONFIG'.
clGetDeviceHalfFPConfig :: OpenCLLibrary -> CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceHalfFPConfig lib = (getDeviceInfoFP lib) . getCLValue $ CL_DEVICE_HALF_FP_CONFIG

-- | Is 'True' if images are supported by the OpenCL device and 'False' otherwise.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_IMAGE_SUPPORT'.
clGetDeviceImageSupport :: OpenCLLibrary -> CLDeviceID -> IO Bool
clGetDeviceImageSupport lib = (getDeviceInfoBool lib) . getCLValue $ CL_DEVICE_IMAGE_SUPPORT

-- | Max height of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_IMAGE2D_MAX_HEIGHT'.
clGetDeviceImage2DMaxHeight :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceImage2DMaxHeight lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_IMAGE2D_MAX_HEIGHT

-- | Max width of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_IMAGE2D_MAX_WIDTH'.
clGetDeviceImage2DMaxWidth :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceImage2DMaxWidth lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_IMAGE2D_MAX_WIDTH

-- | Max depth of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_IMAGE3D_MAX_DEPTH'.
clGetDeviceImage3DMaxDepth :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceImage3DMaxDepth lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_IMAGE3D_MAX_DEPTH

-- | Max height of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_IMAGE3D_MAX_HEIGHT'.
clGetDeviceImage3DMaxHeight :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceImage3DMaxHeight lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_IMAGE3D_MAX_HEIGHT

-- | Max width of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_IMAGE3D_MAX_WIDTH'.
clGetDeviceImage3DMaxWidth :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceImage3DMaxWidth lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_IMAGE3D_MAX_WIDTH

-- | Size of local memory arena in bytes. The minimum value is 16 KB.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_LOCAL_MEM_SIZE'.
clGetDeviceLocalMemSize :: OpenCLLibrary -> CLDeviceID -> IO CLulong
clGetDeviceLocalMemSize lib = (getDeviceInfoUlong lib) . getCLValue $ CL_DEVICE_LOCAL_MEM_SIZE

-- | Type of local memory supported. This can be set to 'CL_LOCAL' implying 
-- dedicated local memory storage such as SRAM, or 'CL_GLOBAL'.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_LOCAL_MEM_TYPE'.
clGetDeviceLocalMemType :: OpenCLLibrary -> CLDeviceID -> IO CLDeviceLocalMemType
clGetDeviceLocalMemType lib device = do
  typ <- getDeviceInfoUint lib (getCLValue CL_DEVICE_LOCAL_MEM_TYPE) device 
  return . getEnumCL $ typ

-- | Maximum configured clock frequency of the device in MHz.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_CLOCK_FREQUENCY'.
clGetDeviceMaxClockFrequency :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxClockFrequency lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_CLOCK_FREQUENCY

-- | The number of parallel compute cores on the OpenCL device. The minimum 
-- value is 1.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_COMPUTE_UNITS'.
clGetDeviceMaxComputeUnits :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxComputeUnits lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_COMPUTE_UNITS

-- | Max number of arguments declared with the __constant qualifier in a kernel. 
-- The minimum value is 8.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_CONSTANT_ARGS'.
clGetDeviceMaxConstantArgs :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxConstantArgs lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_CONSTANT_ARGS

-- | Max size in bytes of a constant buffer allocation. The minimum value is 
-- 64 KB.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE'.
clGetDeviceMaxConstantBufferSize :: OpenCLLibrary -> CLDeviceID -> IO CLulong
clGetDeviceMaxConstantBufferSize lib = (getDeviceInfoUlong lib) . getCLValue $ CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE

-- | Max size of memory object allocation in bytes. The minimum value is max 
-- (1/4th of 'clGetDeviceGlobalMemSize', 128*1024*1024)
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_MAX_MEM_ALLOC_SIZE'.
clGetDeviceMaxMemAllocSize :: OpenCLLibrary -> CLDeviceID -> IO CLulong
clGetDeviceMaxMemAllocSize lib = (getDeviceInfoUlong lib) . getCLValue $ CL_DEVICE_MAX_MEM_ALLOC_SIZE

-- | Max size in bytes of the arguments that can be passed to a kernel. The 
-- minimum value is 256.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_PARAMETER_SIZE'.
clGetDeviceMaxParameterSize :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceMaxParameterSize lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_MAX_PARAMETER_SIZE

-- | Max number of simultaneous image objects that can be read by a kernel. The 
-- minimum value is 128 if 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_READ_IMAGE_ARGS'.
clGetDeviceMaxReadImageArgs :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxReadImageArgs lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_READ_IMAGE_ARGS

-- | Maximum number of samplers that can be used in a kernel. The minimum value 
-- is 16 if 'clGetDeviceImageSupport' is 'True'. (Also see sampler type.)
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_MAX_SAMPLERS'.
clGetDeviceMaxSamplers :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxSamplers lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_SAMPLERS

-- | Maximum number of work-items in a work-group executing a kernel using the 
-- data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). The 
-- minimum value is 1.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_WORK_GROUP_SIZE'.
clGetDeviceMaxWorkGroupSize :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceMaxWorkGroupSize lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_MAX_WORK_GROUP_SIZE

-- | Maximum dimensions that specify the global and local work-item IDs used by 
-- the data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). 
-- The minimum value is 3.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS'.
clGetDeviceMaxWorkItemDimensions :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxWorkItemDimensions lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS

-- | Maximum number of work-items that can be specified in each dimension of the 
-- work-group to 'clEnqueueNDRangeKernel'.
-- Returns n entries, where n is the value returned by the query for 
-- clDeviceMaxWorkItemDimensions. The minimum value is (1, 1, 1).
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_WORK_ITEM_SIZES'.
clGetDeviceMaxWorkItemSizes :: OpenCLLibrary -> CLDeviceID -> IO [CSize]
clGetDeviceMaxWorkItemSizes lib device = do
  n <- clGetDeviceMaxWorkItemDimensions lib device
  allocaArray (fromIntegral n) $ \(buff :: Ptr CSize) -> do
    whenSuccess (raw_clGetDeviceInfo lib device infoid (size n) (castPtr buff) nullPtr)
      $ peekArray (fromIntegral n) buff
    where
      infoid = getCLValue CL_DEVICE_MAX_WORK_ITEM_SIZES
      size n = fromIntegral $ (fromIntegral n) * (sizeOf (0::CSize))

-- | Max number of simultaneous image objects that can be written to by a 
-- kernel. The minimum value is 8 if 'clGetDeviceImageSupport' is 'True'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MAX_WRITE_IMAGE_ARGS'.
clGetDeviceMaxWriteImageArgs :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMaxWriteImageArgs lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MAX_WRITE_IMAGE_ARGS

-- | Describes the alignment in bits of the base address of any allocated 
-- memory object.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MEM_BASE_ADDR_ALIGN'.
clGetDeviceMemBaseAddrAlign :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMemBaseAddrAlign lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MEM_BASE_ADDR_ALIGN

-- | The smallest alignment in bytes which can be used for any data type.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE'.
clGetDeviceMinDataTypeAlignSize :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceMinDataTypeAlignSize lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE

-- | Device name string.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_NAME'.
clGetDeviceName :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceName lib = (getDeviceInfoString lib) . getCLValue $ CL_DEVICE_NAME

-- | The platform associated with this device.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_PLATFORM'.
clGetDevicePlatform :: OpenCLLibrary -> CLDeviceID -> IO CLPlatformID
clGetDevicePlatform lib device = alloca $ \(dat :: Ptr CLPlatformID) -> do
  whenSuccess (raw_clGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      infoid = getCLValue CL_DEVICE_PLATFORM
      size = fromIntegral $ sizeOf (nullPtr :: CLPlatformID)

-- | Preferred native vector width size for built-in char types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR'.
clGetDevicePreferredVectorWidthChar :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthChar lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR

-- | Preferred native vector width size for built-in short types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT'.
clGetDevicePreferredVectorWidthShort :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthShort lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT

-- | Preferred native vector width size for built-in int types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT'.
clGetDevicePreferredVectorWidthInt :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthInt lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT

-- | Preferred native vector width size for built-in long types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG'.
clGetDevicePreferredVectorWidthLong :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthLong lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG

-- | Preferred native vector width size for built-in float types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT'.
clGetDevicePreferredVectorWidthFloat :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthFloat lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT

-- | Preferred native vector width size for built-in double types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
-- | If the cl_khr_fp64 extension is not supported, 
-- 'clGetDevicePreferredVectorWidthDouble' must return 0.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE'.
clGetDevicePreferredVectorWidthDouble :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDevicePreferredVectorWidthDouble lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE

-- | OpenCL profile string. Returns the profile name supported by the device 
-- (see note). The profile name returned can be one of the following strings:
-- 
-- * FULL_PROFILE - if the device supports the OpenCL specification 
-- (functionality defined as part of the core specification and does not require 
-- any extensions to be supported).
-- 
-- * EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_PROFILE'.
clGetDeviceProfile :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceProfile lib = (getDeviceInfoString lib) . getCLValue $ CL_DEVICE_PROFILE

-- | Describes the resolution of device timer. This is measured in nanoseconds.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_PROFILING_TIMER_RESOLUTION'.
clGetDeviceProfilingTimerResolution :: OpenCLLibrary -> CLDeviceID -> IO CSize
clGetDeviceProfilingTimerResolution lib = (getDeviceInfoSizet lib) . getCLValue $ CL_DEVICE_PROFILING_TIMER_RESOLUTION

-- | Describes the command-queue properties supported by the device. This is a 
-- list that describes one or more of the CLCommandQueueProperty values.
-- These properties are described in the table for 'clCreateCommandQueue'. 
-- The mandated minimum capability is 'CL_QUEUE_PROFILING_ENABLE'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_QUEUE_PROPERTIES'.
clGetDeviceQueueProperties :: OpenCLLibrary -> CLDeviceID -> IO [CLCommandQueueProperty]
clGetDeviceQueueProperties lib device = do
  flags <- getDeviceInfoUlong lib (getCLValue CL_DEVICE_QUEUE_PROPERTIES) device
  return . bitmaskToCommandQueueProperties $ flags
    
-- | Describes single precision floating-point capability of the device. This is 
-- a bit-field that describes one or more of the 'CLDeviceFPConfig' values.
-- The mandated minimum floating-point capability is 'CL_FP_ROUND_TO_NEAREST' | 
-- 'CL_FP_INF_NAN'.
--
-- This function execute OpenCL clGetDeviceInfo with
-- 'CL_DEVICE_SINGLE_FP_CONFIG'.
clGetDeviceSingleFPConfig :: OpenCLLibrary -> CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceSingleFPConfig lib = (getDeviceInfoFP lib) . getCLValue $ CL_DEVICE_SINGLE_FP_CONFIG

-- | The OpenCL device type. Currently supported values are one of or a 
-- combination of: 'CL_DEVICE_TYPE_CPU', 'CL_DEVICE_TYPE_GPU', 
-- 'CL_DEVICE_TYPE_ACCELERATOR', or 'CL_DEVICE_TYPE_DEFAULT'.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_TYPE'.
clGetDeviceType :: OpenCLLibrary -> CLDeviceID -> IO [CLDeviceType]
clGetDeviceType lib device = do
  flags <- getDeviceInfoUlong lib (getCLValue CL_DEVICE_TYPE) device
  return . bitmaskToDeviceTypes $ flags

-- | Vendor name string.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_VENDOR'.
clGetDeviceVendor :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceVendor lib = (getDeviceInfoString lib) . getCLValue $ CL_DEVICE_VENDOR

-- | A unique device vendor identifier. An example of a unique device identifier 
-- could be the PCIe ID.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_VENDOR_ID'.
clGetDeviceVendorID :: OpenCLLibrary -> CLDeviceID -> IO CLuint
clGetDeviceVendorID lib = (getDeviceInfoUint lib) . getCLValue $ CL_DEVICE_VENDOR_ID

-- | OpenCL version string. Returns the OpenCL version supported by the device. 
-- This version string has the following format:
-- /OpenCL major_version.minor_version vendor-specific information/
-- The major_version.minor_version value returned will be 1.0.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_VERSION'.
clGetDeviceVersion :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceVersion lib = (getDeviceInfoString lib) . getCLValue $ CL_DEVICE_VERSION

-- | OpenCL software driver version string in the form major_number.minor_number.
--
-- This function execute OpenCL clGetDeviceInfo with 'CL_DRIVER_VERSION'.
clGetDeviceDriverVersion :: OpenCLLibrary -> CLDeviceID -> IO String
clGetDeviceDriverVersion lib = (getDeviceInfoString lib) . getCLValue $ CL_DRIVER_VERSION

-- -----------------------------------------------------------------------------
