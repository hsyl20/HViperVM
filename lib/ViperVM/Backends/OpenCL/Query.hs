{-# LANGUAGE ScopedTypeVariables #-}
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
  whenSuccess, wrapCheckSuccess, bitmaskToFPConfig, bitmaskToExecCapability )

-- -----------------------------------------------------------------------------

getNumPlatforms :: OpenCLLibrary -> IO CLuint
getNumPlatforms lib = alloca $ \(num_platforms :: Ptr CLuint) -> do 
  err <- wrapCheckSuccess (rawClGetPlatformIDs lib 0 nullPtr num_platforms)
  if err
    then peek num_platforms
    else return 0 -- the ICD may return an error CL_PLATFORM_NOT_FOUND_KHR...

-- | Obtain the list of platforms available. Returns the list if the function 
-- is executed successfully. Otherwise it returns the empty list.
clGetPlatformIDs :: OpenCLLibrary -> IO [CLPlatformID]
clGetPlatformIDs lib = do
  nplats <- getNumPlatforms lib
  if nplats == 0
    then return []
    else allocaArray (fromIntegral nplats) $ \(plats :: Ptr CLPlatformID) -> do
           whenSuccess(rawClGetPlatformIDs lib nplats plats nullPtr)
             $ peekArray (fromIntegral nplats) plats
  
getPlatformInfoSize :: OpenCLLibrary -> CLPlatformID -> CLuint -> IO CSize
getPlatformInfoSize lib platform infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (rawClGetPlatformInfo lib platform infoid 0 nullPtr value_size) 
    $ peek value_size
  
-- | Get specific information about the OpenCL platform. It returns Nothing if
-- platform is not a valid platform.
clGetPlatformInfo :: OpenCLLibrary -> CLPlatformInfo -> CLPlatformID -> IO String
clGetPlatformInfo lib infoid platform = do
  sval <- getPlatformInfoSize lib platform infocl
  allocaArray (fromIntegral sval) $ \(buff :: CString) -> do
    whenSuccess (rawClGetPlatformInfo lib platform infocl sval (castPtr buff) nullPtr)
      $ peekCString buff
    where
      infocl = getCLValue infoid

getNumDevices :: OpenCLLibrary -> CLPlatformID -> CLDeviceType_ -> IO CLuint
getNumDevices lib platform dtype = alloca $ \(num_devices :: Ptr CLuint) -> do
  whenSuccess (rawClGetDeviceIDs lib platform dtype 0 nullPtr num_devices)
    $ peek num_devices

-- | Obtain the list of devices available on a platform. Returns the list if 
-- the function is executed successfully. Otherwise it returns the empty list 
-- if platform is not a valid platform or no OpenCL devices that matched 
-- device_type were found.
clGetDeviceIDs :: OpenCLLibrary -> CLDeviceType -> CLPlatformID -> IO [CLDeviceID]
clGetDeviceIDs lib dtype platform = do
  ndevs <- getNumDevices lib platform dval
  allocaArray (fromIntegral ndevs) $ \(devs :: Ptr CLDeviceID) -> do
    whenSuccess (rawClGetDeviceIDs lib platform dval ndevs devs nullPtr)
      $ peekArray (fromIntegral ndevs) devs
    where
      dval = getCLValue dtype

getDeviceInfoSize :: OpenCLLibrary -> CLDeviceID -> CLDeviceInfo_ -> IO CSize
getDeviceInfoSize lib device infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (rawClGetDeviceInfo lib device infoid 0 nullPtr value_size)
    $ peek value_size
  
getDeviceInfoString :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO String
getDeviceInfoString lib infoid device = do
  n <- getDeviceInfoSize lib device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (rawClGetDeviceInfo lib device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
  
getDeviceInfoUint :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CLuint
getDeviceInfoUint lib infoid device = alloca $ \(dat :: Ptr CLuint) -> do
  whenSuccess (rawClGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CLuint)

getDeviceInfoUlong :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CLulong
getDeviceInfoUlong lib infoid device = alloca $ \(dat :: Ptr CLulong) -> do
  whenSuccess (rawClGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CLulong)

getDeviceInfoSizet :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO CSize
getDeviceInfoSizet lib infoid device = alloca $ \(dat :: Ptr CSize) -> do
  whenSuccess (rawClGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat
    where 
      size = fromIntegral $ sizeOf (0::CSize)
  
getDeviceInfoBool :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO Bool
getDeviceInfoBool lib infoid device = alloca $ \(dat :: Ptr CLbool) -> do
  whenSuccess (rawClGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
    $ peek dat >>= return . (/=0)
    where 
      size = fromIntegral $ sizeOf (0::CLbool)
  
getDeviceInfoFP :: OpenCLLibrary -> CLDeviceInfo_ -> CLDeviceID -> IO [CLDeviceFPConfig]
getDeviceInfoFP lib infoid device = do
  flags <- getDeviceInfoUlong lib infoid device
  return . bitmaskToFPConfig $ flags

data CLDeviceInfo =
     CL_DEVICE_TYPE
   | CL_DEVICE_VENDOR_ID
   | CL_DEVICE_MAX_COMPUTE_UNITS
   | CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
   | CL_DEVICE_MAX_WORK_GROUP_SIZE
   | CL_DEVICE_MAX_WORK_ITEM_SIZES
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE
   | CL_DEVICE_MAX_CLOCK_FREQUENCY
   | CL_DEVICE_ADDRESS_BITS
   | CL_DEVICE_MAX_READ_IMAGE_ARGS
   | CL_DEVICE_MAX_WRITE_IMAGE_ARGS
   | CL_DEVICE_MAX_MEM_ALLOC_SIZE
   | CL_DEVICE_IMAGE2D_MAX_WIDTH
   | CL_DEVICE_IMAGE2D_MAX_HEIGHT
   | CL_DEVICE_IMAGE3D_MAX_WIDTH
   | CL_DEVICE_IMAGE3D_MAX_HEIGHT
   | CL_DEVICE_IMAGE3D_MAX_DEPTH
   | CL_DEVICE_IMAGE_SUPPORT
   | CL_DEVICE_MAX_PARAMETER_SIZE
   | CL_DEVICE_MAX_SAMPLERS
   | CL_DEVICE_MEM_BASE_ADDR_ALIGN
   | CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE
   | CL_DEVICE_SINGLE_FP_CONFIG
   | CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
   | CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
   | CL_DEVICE_GLOBAL_MEM_CACHE_SIZE
   | CL_DEVICE_GLOBAL_MEM_SIZE
   | CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
   | CL_DEVICE_MAX_CONSTANT_ARGS
   | CL_DEVICE_LOCAL_MEM_TYPE
   | CL_DEVICE_LOCAL_MEM_SIZE
   | CL_DEVICE_ERROR_CORRECTION_SUPPORT
   | CL_DEVICE_PROFILING_TIMER_RESOLUTION
   | CL_DEVICE_ENDIAN_LITTLE
   | CL_DEVICE_AVAILABLE
   | CL_DEVICE_COMPILER_AVAILABLE
   | CL_DEVICE_EXECUTION_CAPABILITIES
   | CL_DEVICE_QUEUE_PROPERTIES
   | CL_DEVICE_NAME
   | CL_DEVICE_VENDOR
   | CL_DRIVER_VERSION
   | CL_DEVICE_PROFILE
   | CL_DEVICE_VERSION
   | CL_DEVICE_EXTENSIONS
   | CL_DEVICE_PLATFORM
   | CL_DEVICE_DOUBLE_FP_CONFIG
   | CL_DEVICE_HALF_FP_CONFIG
   | CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF
   | CL_DEVICE_HOST_UNIFIED_MEMORY
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_INT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE
   | CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF
   | CL_DEVICE_OPENCL_C_VERSION
   | CL_DEVICE_LINKER_AVAILABLE
   | CL_DEVICE_BUILT_IN_KERNELS
   | CL_DEVICE_IMAGE_MAX_BUFFER_SIZE
   | CL_DEVICE_IMAGE_MAX_ARRAY_SIZE
   | CL_DEVICE_PARENT_DEVICE
   | CL_DEVICE_PARTITION_MAX_SUB_DEVICES
   | CL_DEVICE_PARTITION_PROPERTIES
   | CL_DEVICE_PARTITION_AFFINITY_DOMAIN
   | CL_DEVICE_PARTITION_TYPE
   | CL_DEVICE_REFERENCE_COUNT
   | CL_DEVICE_PREFERRED_INTEROP_USER_SYNC
   | CL_DEVICE_PRINTF_BUFFER_SIZE
   | CL_DEVICE_IMAGE_PITCH_ALIGNMENT
   | CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT

instance Enum CLDeviceInfo where
   fromEnum CL_DEVICE_TYPE                             = 0x1000
   fromEnum CL_DEVICE_VENDOR_ID                        = 0x1001
   fromEnum CL_DEVICE_MAX_COMPUTE_UNITS                = 0x1002
   fromEnum CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS         = 0x1003
   fromEnum CL_DEVICE_MAX_WORK_GROUP_SIZE              = 0x1004
   fromEnum CL_DEVICE_MAX_WORK_ITEM_SIZES              = 0x1005
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR      = 0x1006
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT     = 0x1007
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT       = 0x1008
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG      = 0x1009
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT     = 0x100A
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE    = 0x100B
   fromEnum CL_DEVICE_MAX_CLOCK_FREQUENCY              = 0x100C
   fromEnum CL_DEVICE_ADDRESS_BITS                     = 0x100D
   fromEnum CL_DEVICE_MAX_READ_IMAGE_ARGS              = 0x100E
   fromEnum CL_DEVICE_MAX_WRITE_IMAGE_ARGS             = 0x100F
   fromEnum CL_DEVICE_MAX_MEM_ALLOC_SIZE               = 0x1010
   fromEnum CL_DEVICE_IMAGE2D_MAX_WIDTH                = 0x1011
   fromEnum CL_DEVICE_IMAGE2D_MAX_HEIGHT               = 0x1012
   fromEnum CL_DEVICE_IMAGE3D_MAX_WIDTH                = 0x1013
   fromEnum CL_DEVICE_IMAGE3D_MAX_HEIGHT               = 0x1014
   fromEnum CL_DEVICE_IMAGE3D_MAX_DEPTH                = 0x1015
   fromEnum CL_DEVICE_IMAGE_SUPPORT                    = 0x1016
   fromEnum CL_DEVICE_MAX_PARAMETER_SIZE               = 0x1017
   fromEnum CL_DEVICE_MAX_SAMPLERS                     = 0x1018
   fromEnum CL_DEVICE_MEM_BASE_ADDR_ALIGN              = 0x1019
   fromEnum CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE         = 0x101A
   fromEnum CL_DEVICE_SINGLE_FP_CONFIG                 = 0x101B
   fromEnum CL_DEVICE_GLOBAL_MEM_CACHE_TYPE            = 0x101C
   fromEnum CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE        = 0x101D
   fromEnum CL_DEVICE_GLOBAL_MEM_CACHE_SIZE            = 0x101E
   fromEnum CL_DEVICE_GLOBAL_MEM_SIZE                  = 0x101F
   fromEnum CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE         = 0x1020
   fromEnum CL_DEVICE_MAX_CONSTANT_ARGS                = 0x1021
   fromEnum CL_DEVICE_LOCAL_MEM_TYPE                   = 0x1022
   fromEnum CL_DEVICE_LOCAL_MEM_SIZE                   = 0x1023
   fromEnum CL_DEVICE_ERROR_CORRECTION_SUPPORT         = 0x1024
   fromEnum CL_DEVICE_PROFILING_TIMER_RESOLUTION       = 0x1025
   fromEnum CL_DEVICE_ENDIAN_LITTLE                    = 0x1026
   fromEnum CL_DEVICE_AVAILABLE                        = 0x1027
   fromEnum CL_DEVICE_COMPILER_AVAILABLE               = 0x1028
   fromEnum CL_DEVICE_EXECUTION_CAPABILITIES           = 0x1029
   fromEnum CL_DEVICE_QUEUE_PROPERTIES                 = 0x102A
   fromEnum CL_DEVICE_NAME                             = 0x102B
   fromEnum CL_DEVICE_VENDOR                           = 0x102C
   fromEnum CL_DRIVER_VERSION                          = 0x102D
   fromEnum CL_DEVICE_PROFILE                          = 0x102E
   fromEnum CL_DEVICE_VERSION                          = 0x102F
   fromEnum CL_DEVICE_EXTENSIONS                       = 0x1030
   fromEnum CL_DEVICE_PLATFORM                         = 0x1031
   fromEnum CL_DEVICE_DOUBLE_FP_CONFIG                 = 0x1032
   fromEnum CL_DEVICE_HALF_FP_CONFIG                   = 0x1033
   fromEnum CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF      = 0x1034
   fromEnum CL_DEVICE_HOST_UNIFIED_MEMORY              = 0x1035
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR         = 0x1036
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT        = 0x1037
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_INT          = 0x1038
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG         = 0x1039
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT        = 0x103A
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE       = 0x103B
   fromEnum CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF         = 0x103C
   fromEnum CL_DEVICE_OPENCL_C_VERSION                 = 0x103D
   fromEnum CL_DEVICE_LINKER_AVAILABLE                 = 0x103E
   fromEnum CL_DEVICE_BUILT_IN_KERNELS                 = 0x103F
   fromEnum CL_DEVICE_IMAGE_MAX_BUFFER_SIZE            = 0x1040
   fromEnum CL_DEVICE_IMAGE_MAX_ARRAY_SIZE             = 0x1041
   fromEnum CL_DEVICE_PARENT_DEVICE                    = 0x1042
   fromEnum CL_DEVICE_PARTITION_MAX_SUB_DEVICES        = 0x1043
   fromEnum CL_DEVICE_PARTITION_PROPERTIES             = 0x1044
   fromEnum CL_DEVICE_PARTITION_AFFINITY_DOMAIN        = 0x1045
   fromEnum CL_DEVICE_PARTITION_TYPE                   = 0x1046
   fromEnum CL_DEVICE_REFERENCE_COUNT                  = 0x1047
   fromEnum CL_DEVICE_PREFERRED_INTEROP_USER_SYNC      = 0x1048
   fromEnum CL_DEVICE_PRINTF_BUFFER_SIZE               = 0x1049
   fromEnum CL_DEVICE_IMAGE_PITCH_ALIGNMENT            = 0x104A
   fromEnum CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT     = 0x104B

   toEnum 0x1000 = CL_DEVICE_TYPE
   toEnum 0x1001 = CL_DEVICE_VENDOR_ID
   toEnum 0x1002 = CL_DEVICE_MAX_COMPUTE_UNITS
   toEnum 0x1003 = CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
   toEnum 0x1004 = CL_DEVICE_MAX_WORK_GROUP_SIZE
   toEnum 0x1005 = CL_DEVICE_MAX_WORK_ITEM_SIZES
   toEnum 0x1006 = CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR
   toEnum 0x1007 = CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT
   toEnum 0x1008 = CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT
   toEnum 0x1009 = CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG
   toEnum 0x100A = CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT
   toEnum 0x100B = CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE
   toEnum 0x100C = CL_DEVICE_MAX_CLOCK_FREQUENCY
   toEnum 0x100D = CL_DEVICE_ADDRESS_BITS
   toEnum 0x100E = CL_DEVICE_MAX_READ_IMAGE_ARGS
   toEnum 0x100F = CL_DEVICE_MAX_WRITE_IMAGE_ARGS
   toEnum 0x1010 = CL_DEVICE_MAX_MEM_ALLOC_SIZE
   toEnum 0x1011 = CL_DEVICE_IMAGE2D_MAX_WIDTH
   toEnum 0x1012 = CL_DEVICE_IMAGE2D_MAX_HEIGHT
   toEnum 0x1013 = CL_DEVICE_IMAGE3D_MAX_WIDTH
   toEnum 0x1014 = CL_DEVICE_IMAGE3D_MAX_HEIGHT
   toEnum 0x1015 = CL_DEVICE_IMAGE3D_MAX_DEPTH
   toEnum 0x1016 = CL_DEVICE_IMAGE_SUPPORT
   toEnum 0x1017 = CL_DEVICE_MAX_PARAMETER_SIZE
   toEnum 0x1018 = CL_DEVICE_MAX_SAMPLERS
   toEnum 0x1019 = CL_DEVICE_MEM_BASE_ADDR_ALIGN
   toEnum 0x101A = CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE
   toEnum 0x101B = CL_DEVICE_SINGLE_FP_CONFIG
   toEnum 0x101C = CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
   toEnum 0x101D = CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
   toEnum 0x101E = CL_DEVICE_GLOBAL_MEM_CACHE_SIZE
   toEnum 0x101F = CL_DEVICE_GLOBAL_MEM_SIZE
   toEnum 0x1020 = CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
   toEnum 0x1021 = CL_DEVICE_MAX_CONSTANT_ARGS
   toEnum 0x1022 = CL_DEVICE_LOCAL_MEM_TYPE
   toEnum 0x1023 = CL_DEVICE_LOCAL_MEM_SIZE
   toEnum 0x1024 = CL_DEVICE_ERROR_CORRECTION_SUPPORT
   toEnum 0x1025 = CL_DEVICE_PROFILING_TIMER_RESOLUTION
   toEnum 0x1026 = CL_DEVICE_ENDIAN_LITTLE
   toEnum 0x1027 = CL_DEVICE_AVAILABLE
   toEnum 0x1028 = CL_DEVICE_COMPILER_AVAILABLE
   toEnum 0x1029 = CL_DEVICE_EXECUTION_CAPABILITIES
   toEnum 0x102A = CL_DEVICE_QUEUE_PROPERTIES
   toEnum 0x102B = CL_DEVICE_NAME
   toEnum 0x102C = CL_DEVICE_VENDOR
   toEnum 0x102D = CL_DRIVER_VERSION
   toEnum 0x102E = CL_DEVICE_PROFILE
   toEnum 0x102F = CL_DEVICE_VERSION
   toEnum 0x1030 = CL_DEVICE_EXTENSIONS
   toEnum 0x1031 = CL_DEVICE_PLATFORM
   toEnum 0x1032 = CL_DEVICE_DOUBLE_FP_CONFIG
   toEnum 0x1033 = CL_DEVICE_HALF_FP_CONFIG
   toEnum 0x1034 = CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF
   toEnum 0x1035 = CL_DEVICE_HOST_UNIFIED_MEMORY
   toEnum 0x1036 = CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR
   toEnum 0x1037 = CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT
   toEnum 0x1038 = CL_DEVICE_NATIVE_VECTOR_WIDTH_INT
   toEnum 0x1039 = CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG
   toEnum 0x103A = CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT
   toEnum 0x103B = CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE
   toEnum 0x103C = CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF
   toEnum 0x103D = CL_DEVICE_OPENCL_C_VERSION
   toEnum 0x103E = CL_DEVICE_LINKER_AVAILABLE
   toEnum 0x103F = CL_DEVICE_BUILT_IN_KERNELS
   toEnum 0x1040 = CL_DEVICE_IMAGE_MAX_BUFFER_SIZE
   toEnum 0x1041 = CL_DEVICE_IMAGE_MAX_ARRAY_SIZE
   toEnum 0x1042 = CL_DEVICE_PARENT_DEVICE
   toEnum 0x1043 = CL_DEVICE_PARTITION_MAX_SUB_DEVICES
   toEnum 0x1044 = CL_DEVICE_PARTITION_PROPERTIES
   toEnum 0x1045 = CL_DEVICE_PARTITION_AFFINITY_DOMAIN
   toEnum 0x1046 = CL_DEVICE_PARTITION_TYPE
   toEnum 0x1047 = CL_DEVICE_REFERENCE_COUNT
   toEnum 0x1048 = CL_DEVICE_PREFERRED_INTEROP_USER_SYNC
   toEnum 0x1049 = CL_DEVICE_PRINTF_BUFFER_SIZE
   toEnum 0x104A = CL_DEVICE_IMAGE_PITCH_ALIGNMENT
   toEnum 0x104B = CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT
   toEnum _ = error "Invalid Device Info value"

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

-- | This function execute OpenCL clGetDeviceInfo with 'CL_DEVICE_EXTENSIONS'.
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
    whenSuccess (rawClGetDeviceInfo lib device infoid (size n) (castPtr buff) nullPtr)
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
  whenSuccess (rawClGetDeviceInfo lib device infoid size (castPtr dat) nullPtr)
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
