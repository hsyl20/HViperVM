{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, CPP #-}
module ViperVM.Backends.OpenCL.Types( 
  -- * Symple CL Types
  CLbool, CLint, CLuint, CLulong, CLProgram, CLEvent, CLMem, CLPlatformID, 
  CLDeviceID, CLContext, CLCommandQueue, CLPlatformInfo_, CLDeviceType_, 
  CLDeviceInfo_, CLContextInfo_, CLContextProperty_, CLCommandQueueInfo_, 
  CLCommandQueueInfo(..),
  CLEventInfo_, CLProfilingInfo_, CLCommandType_, CLCommandQueueProperty_, 
  CLMemFlags_, CLMemObjectType_, CLMemInfo_, CLImageInfo_, CLMapFlags_,
  CLProgramInfo_, CLBuildStatus_,CLKernel, CLProgramBuildInfo_, CLKernelInfo_,
  CLKernelWorkGroupInfo_, CLDeviceLocalMemType_, CLDeviceMemCacheType_,
  CLSampler, CLFilterMode_, CLSamplerInfo_, CLAddressingMode_,
  -- * Memory Types
  CLImageFormat(..), CLImageFormat_p, CLChannelOrder(..), CLChannelType(..),
  -- * High Level Types
  CLError(..), CLDeviceFPConfig(..), CLDeviceMemCacheType(..), 
  CLDeviceExecCapability(..), CLDeviceLocalMemType(..), CLDeviceType(..), 
  CLCommandQueueProperty(..), CLCommandType(..),  CLCommandExecutionStatus(..), 
  CLProfilingInfo(..), CLPlatformInfo(..), CLMemFlag(..), CLMemObjectType(..),
  CLBuildStatus(..), CLAddressingMode(..), CLFilterMode(..), CLMapFlag(..),
  ContextCallback, NativeKernelCallback, BuildCallback,
  -- * Functions
  wrapPError, wrapCheckSuccess, wrapGetInfo, whenSuccess, getCLValue, wrapContextCallback,
  wrapNativeKernelCallback, withMaybeArray,
  throwCLError, getEnumCL, bitmaskToFlags, getCommandExecutionStatus, 
  bitmaskToDeviceTypes, bitmaskFromFlags, bitmaskToCommandQueueProperties, 
  bitmaskToFPConfig, bitmaskToExecCapability, bitmaskToMemFlags )
       where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Foreign.C.String( CString )
import Data.List( foldl' )
import Data.Typeable( Typeable(..) )
import Control.Applicative( (<$>) , (<*>))
import Control.Exception( Exception(..), throwIO )

type CLPlatformID = Ptr ()
type CLDeviceID = Ptr ()
type CLContext = Ptr ()
type CLCommandQueue = Ptr ()
type CLMem = Ptr ()
type CLEvent = Ptr ()
type CLProgram = Ptr ()
type CLKernel = Ptr ()
type CLSampler = Ptr ()

type CLint = Int32
type CLuint = Word32
type CLulong = Word64
type CLbool = CLuint
type CLbitfield = CLulong

type CLPlatformInfo_ = CLuint
type CLDeviceType_ = CLbitfield
type CLDeviceInfo_ = CLuint
type CLDeviceFPConfig_ = CLbitfield
type CLDeviceMemCacheType_ = CLuint
type CLDeviceLocalMemType_ = CLuint
type CLDeviceExecCapability_ = CLbitfield
type CLContextInfo_ = CLuint
type CLContextProperty_ = IntPtr
type CLCommandQueueInfo_ = CLuint
type CLCommandQueueProperty_ = CLbitfield
type CLEventInfo_ = CLuint
type CLProfilingInfo_ = CLuint
type CLCommandType_ = CLuint
type CLMemFlags_ = CLbitfield
type CLMemObjectType_ = CLuint
type CLMemInfo_ = CLuint
type CLImageInfo_ = CLuint
type CLMapFlags_ = CLbitfield
type CLProgramInfo_ = CLuint
type CLProgramBuildInfo_ = CLuint
type CLBuildStatus_ = CLint
type CLKernelInfo_ = CLuint
type CLKernelWorkGroupInfo_ = CLuint
type CLFilterMode_ = CLuint
type CLSamplerInfo_ = CLuint
type CLAddressingMode_ = CLuint

data CLError =
     CL_SUCCESS                                  
   | CL_DEVICE_NOT_FOUND                         
   | CL_DEVICE_NOT_AVAILABLE                     
   | CL_COMPILER_NOT_AVAILABLE                   
   | CL_MEM_OBJECT_ALLOCATION_FAILURE            
   | CL_OUT_OF_RESOURCES                         
   | CL_OUT_OF_HOST_MEMORY                       
   | CL_PROFILING_INFO_NOT_AVAILABLE             
   | CL_MEM_COPY_OVERLAP                         
   | CL_IMAGE_FORMAT_MISMATCH                    
   | CL_IMAGE_FORMAT_NOT_SUPPORTED               
   | CL_BUILD_PROGRAM_FAILURE                    
   | CL_MAP_FAILURE                              
   | CL_MISALIGNED_SUB_BUFFER_OFFSET             
   | CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST
   | CL_COMPILE_PROGRAM_FAILURE                  
   | CL_LINKER_NOT_AVAILABLE                     
   | CL_LINK_PROGRAM_FAILURE                     
   | CL_DEVICE_PARTITION_FAILED                  
   | CL_KERNEL_ARG_INFO_NOT_AVAILABLE            
   | CL_INVALID_VALUE                            
   | CL_INVALID_DEVICE_TYPE                      
   | CL_INVALID_PLATFORM                         
   | CL_INVALID_DEVICE                           
   | CL_INVALID_CONTEXT                          
   | CL_INVALID_QUEUE_PROPERTIES                 
   | CL_INVALID_COMMAND_QUEUE                    
   | CL_INVALID_HOST_PTR                         
   | CL_INVALID_MEM_OBJECT                       
   | CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          
   | CL_INVALID_IMAGE_SIZE                       
   | CL_INVALID_SAMPLER                          
   | CL_INVALID_BINARY                           
   | CL_INVALID_BUILD_OPTIONS                    
   | CL_INVALID_PROGRAM                          
   | CL_INVALID_PROGRAM_EXECUTABLE               
   | CL_INVALID_KERNEL_NAME                      
   | CL_INVALID_KERNEL_DEFINITION                
   | CL_INVALID_KERNEL                           
   | CL_INVALID_ARG_INDEX                        
   | CL_INVALID_ARG_VALUE                        
   | CL_INVALID_ARG_SIZE                         
   | CL_INVALID_KERNEL_ARGS                      
   | CL_INVALID_WORK_DIMENSION                   
   | CL_INVALID_WORK_GROUP_SIZE                  
   | CL_INVALID_WORK_ITEM_SIZE                   
   | CL_INVALID_GLOBAL_OFFSET                    
   | CL_INVALID_EVENT_WAIT_LIST                  
   | CL_INVALID_EVENT                            
   | CL_INVALID_OPERATION                        
   | CL_INVALID_GL_OBJECT                        
   | CL_INVALID_BUFFER_SIZE                      
   | CL_INVALID_MIP_LEVEL                        
   | CL_INVALID_GLOBAL_WORK_SIZE                 
   | CL_INVALID_PROPERTY                         
   | CL_INVALID_IMAGE_DESCRIPTOR                 
   | CL_INVALID_COMPILER_OPTIONS                 
   | CL_INVALID_LINKER_OPTIONS                   
   | CL_INVALID_DEVICE_PARTITION_COUNT           
   deriving (Show, Eq, Typeable)

instance Enum CLError where
   fromEnum CL_SUCCESS                                   = 0
   fromEnum CL_DEVICE_NOT_FOUND                          = -1
   fromEnum CL_DEVICE_NOT_AVAILABLE                      = -2
   fromEnum CL_COMPILER_NOT_AVAILABLE                    = -3
   fromEnum CL_MEM_OBJECT_ALLOCATION_FAILURE             = -4
   fromEnum CL_OUT_OF_RESOURCES                          = -5
   fromEnum CL_OUT_OF_HOST_MEMORY                        = -6
   fromEnum CL_PROFILING_INFO_NOT_AVAILABLE              = -7
   fromEnum CL_MEM_COPY_OVERLAP                          = -8
   fromEnum CL_IMAGE_FORMAT_MISMATCH                     = -9
   fromEnum CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10
   fromEnum CL_BUILD_PROGRAM_FAILURE                     = -11
   fromEnum CL_MAP_FAILURE                               = -12
   fromEnum CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13
   fromEnum CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14
   fromEnum CL_COMPILE_PROGRAM_FAILURE                   = -15
   fromEnum CL_LINKER_NOT_AVAILABLE                      = -16
   fromEnum CL_LINK_PROGRAM_FAILURE                      = -17
   fromEnum CL_DEVICE_PARTITION_FAILED                   = -18
   fromEnum CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19
   fromEnum CL_INVALID_VALUE                             = -30
   fromEnum CL_INVALID_DEVICE_TYPE                       = -31
   fromEnum CL_INVALID_PLATFORM                          = -32
   fromEnum CL_INVALID_DEVICE                            = -33
   fromEnum CL_INVALID_CONTEXT                           = -34
   fromEnum CL_INVALID_QUEUE_PROPERTIES                  = -35
   fromEnum CL_INVALID_COMMAND_QUEUE                     = -36
   fromEnum CL_INVALID_HOST_PTR                          = -37
   fromEnum CL_INVALID_MEM_OBJECT                        = -38
   fromEnum CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           = -39
   fromEnum CL_INVALID_IMAGE_SIZE                        = -40
   fromEnum CL_INVALID_SAMPLER                           = -41
   fromEnum CL_INVALID_BINARY                            = -42
   fromEnum CL_INVALID_BUILD_OPTIONS                     = -43
   fromEnum CL_INVALID_PROGRAM                           = -44
   fromEnum CL_INVALID_PROGRAM_EXECUTABLE                = -45
   fromEnum CL_INVALID_KERNEL_NAME                       = -46
   fromEnum CL_INVALID_KERNEL_DEFINITION                 = -47
   fromEnum CL_INVALID_KERNEL                            = -48
   fromEnum CL_INVALID_ARG_INDEX                         = -49
   fromEnum CL_INVALID_ARG_VALUE                         = -50
   fromEnum CL_INVALID_ARG_SIZE                          = -51
   fromEnum CL_INVALID_KERNEL_ARGS                       = -52
   fromEnum CL_INVALID_WORK_DIMENSION                    = -53
   fromEnum CL_INVALID_WORK_GROUP_SIZE                   = -54
   fromEnum CL_INVALID_WORK_ITEM_SIZE                    = -55
   fromEnum CL_INVALID_GLOBAL_OFFSET                     = -56
   fromEnum CL_INVALID_EVENT_WAIT_LIST                   = -57
   fromEnum CL_INVALID_EVENT                             = -58
   fromEnum CL_INVALID_OPERATION                         = -59
   fromEnum CL_INVALID_GL_OBJECT                         = -60
   fromEnum CL_INVALID_BUFFER_SIZE                       = -61
   fromEnum CL_INVALID_MIP_LEVEL                         = -62
   fromEnum CL_INVALID_GLOBAL_WORK_SIZE                  = -63
   fromEnum CL_INVALID_PROPERTY                          = -64
   fromEnum CL_INVALID_IMAGE_DESCRIPTOR                  = -65
   fromEnum CL_INVALID_COMPILER_OPTIONS                  = -66
   fromEnum CL_INVALID_LINKER_OPTIONS                    = -67
   fromEnum CL_INVALID_DEVICE_PARTITION_COUNT            = -68

   toEnum 0   = CL_SUCCESS
   toEnum (-1)  = CL_DEVICE_NOT_FOUND
   toEnum (-2)  = CL_DEVICE_NOT_AVAILABLE
   toEnum (-3)  = CL_COMPILER_NOT_AVAILABLE
   toEnum (-4)  = CL_MEM_OBJECT_ALLOCATION_FAILURE
   toEnum (-5)  = CL_OUT_OF_RESOURCES
   toEnum (-6)  = CL_OUT_OF_HOST_MEMORY
   toEnum (-7)  = CL_PROFILING_INFO_NOT_AVAILABLE
   toEnum (-8)  = CL_MEM_COPY_OVERLAP
   toEnum (-9)  = CL_IMAGE_FORMAT_MISMATCH
   toEnum (-10) = CL_IMAGE_FORMAT_NOT_SUPPORTED
   toEnum (-11) = CL_BUILD_PROGRAM_FAILURE
   toEnum (-12) = CL_MAP_FAILURE
   toEnum (-13) = CL_MISALIGNED_SUB_BUFFER_OFFSET
   toEnum (-14) = CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST
   toEnum (-15) = CL_COMPILE_PROGRAM_FAILURE
   toEnum (-16) = CL_LINKER_NOT_AVAILABLE
   toEnum (-17) = CL_LINK_PROGRAM_FAILURE
   toEnum (-18) = CL_DEVICE_PARTITION_FAILED
   toEnum (-19) = CL_KERNEL_ARG_INFO_NOT_AVAILABLE
   toEnum (-30) = CL_INVALID_VALUE
   toEnum (-31) = CL_INVALID_DEVICE_TYPE                  
   toEnum (-32) = CL_INVALID_PLATFORM                     
   toEnum (-33) = CL_INVALID_DEVICE                       
   toEnum (-34) = CL_INVALID_CONTEXT                      
   toEnum (-35) = CL_INVALID_QUEUE_PROPERTIES             
   toEnum (-36) = CL_INVALID_COMMAND_QUEUE                
   toEnum (-37) = CL_INVALID_HOST_PTR                     
   toEnum (-38) = CL_INVALID_MEM_OBJECT                   
   toEnum (-39) = CL_INVALID_IMAGE_FORMAT_DESCRIPTOR      
   toEnum (-40) = CL_INVALID_IMAGE_SIZE                   
   toEnum (-41) = CL_INVALID_SAMPLER                      
   toEnum (-42) = CL_INVALID_BINARY                       
   toEnum (-43) = CL_INVALID_BUILD_OPTIONS                
   toEnum (-44) = CL_INVALID_PROGRAM                      
   toEnum (-45) = CL_INVALID_PROGRAM_EXECUTABLE           
   toEnum (-46) = CL_INVALID_KERNEL_NAME                  
   toEnum (-47) = CL_INVALID_KERNEL_DEFINITION            
   toEnum (-48) = CL_INVALID_KERNEL                       
   toEnum (-49) = CL_INVALID_ARG_INDEX                    
   toEnum (-50) = CL_INVALID_ARG_VALUE                    
   toEnum (-51) = CL_INVALID_ARG_SIZE                     
   toEnum (-52) = CL_INVALID_KERNEL_ARGS                  
   toEnum (-53) = CL_INVALID_WORK_DIMENSION               
   toEnum (-54) = CL_INVALID_WORK_GROUP_SIZE              
   toEnum (-55) = CL_INVALID_WORK_ITEM_SIZE               
   toEnum (-56) = CL_INVALID_GLOBAL_OFFSET                
   toEnum (-57) = CL_INVALID_EVENT_WAIT_LIST              
   toEnum (-58) = CL_INVALID_EVENT                        
   toEnum (-59) = CL_INVALID_OPERATION                    
   toEnum (-60) = CL_INVALID_GL_OBJECT                    
   toEnum (-61) = CL_INVALID_BUFFER_SIZE                  
   toEnum (-62) = CL_INVALID_MIP_LEVEL                    
   toEnum (-63) = CL_INVALID_GLOBAL_WORK_SIZE             
   toEnum (-64) = CL_INVALID_PROPERTY                     
   toEnum (-65) = CL_INVALID_IMAGE_DESCRIPTOR             
   toEnum (-66) = CL_INVALID_COMPILER_OPTIONS             
   toEnum (-67) = CL_INVALID_LINKER_OPTIONS               
   toEnum (-68) = CL_INVALID_DEVICE_PARTITION_COUNT       
   toEnum _ = error "Invalid Error value"

instance Exception CLError

throwCLError :: CLint -> IO a
throwCLError = throwIO . (getEnumCL :: CLint -> CLError)

wrapPError :: (Ptr CLint -> IO a) -> IO a
wrapPError f = alloca $ \perr -> do
  v <- f perr
  errcode <- getEnumCL <$> peek perr
  if errcode == CL_SUCCESS
    then return v
    else throwIO errcode
  
wrapCheckSuccess :: IO CLint -> IO Bool
wrapCheckSuccess f = f >>= return . (==CL_SUCCESS) . getEnumCL

wrapGetInfo :: Storable a 
               => (Ptr a -> Ptr CSize -> IO CLint) -> (a -> b) -> IO b
wrapGetInfo fget fconvert= alloca $ \dat -> do
  errcode <- fget dat nullPtr
  if errcode == getCLValue CL_SUCCESS
    then fmap fconvert $ peek dat
    else throwCLError errcode

whenSuccess :: IO CLint -> IO a -> IO a
whenSuccess fcheck fval = do
  errcode <- fcheck
  if errcode == getCLValue CL_SUCCESS
    then fval
    else throwCLError errcode
         
data CLPlatformInfo =
     CL_PLATFORM_PROFILE
   | CL_PLATFORM_VERSION
   | CL_PLATFORM_NAME
   | CL_PLATFORM_VENDOR
   | CL_PLATFORM_EXTENSIONS
   deriving (Show)

instance Enum CLPlatformInfo where
   fromEnum CL_PLATFORM_PROFILE    = 0x0900
   fromEnum CL_PLATFORM_VERSION    = 0x0901
   fromEnum CL_PLATFORM_NAME       = 0x0902
   fromEnum CL_PLATFORM_VENDOR     = 0x0903
   fromEnum CL_PLATFORM_EXTENSIONS = 0x0904

   toEnum 0x0900 = CL_PLATFORM_PROFILE
   toEnum 0x0901 = CL_PLATFORM_VERSION
   toEnum 0x0902 = CL_PLATFORM_NAME
   toEnum 0x0903 = CL_PLATFORM_VENDOR
   toEnum 0x0904 = CL_PLATFORM_EXTENSIONS
   toEnum _ = error "Invalid Platform Info value"


data CLDeviceType = 
     CL_DEVICE_TYPE_DEFAULT
   | CL_DEVICE_TYPE_CPU
   | CL_DEVICE_TYPE_GPU
   | CL_DEVICE_TYPE_ACCELERATOR
   | CL_DEVICE_TYPE_CUSTOM
   | CL_DEVICE_TYPE_ALL
   deriving (Show)

instance Enum CLDeviceType where
   fromEnum CL_DEVICE_TYPE_DEFAULT     = 1
   fromEnum CL_DEVICE_TYPE_CPU         = 2
   fromEnum CL_DEVICE_TYPE_GPU         = 4
   fromEnum CL_DEVICE_TYPE_ACCELERATOR = 8
   fromEnum CL_DEVICE_TYPE_CUSTOM      = 16
   fromEnum CL_DEVICE_TYPE_ALL         = 0xFFFFFFFF

   toEnum 1   = CL_DEVICE_TYPE_DEFAULT
   toEnum 2   = CL_DEVICE_TYPE_CPU
   toEnum 4   = CL_DEVICE_TYPE_GPU
   toEnum 8   = CL_DEVICE_TYPE_ACCELERATOR
   toEnum 16  = CL_DEVICE_TYPE_CUSTOM
   toEnum 0xFFFFFFFF = CL_DEVICE_TYPE_ALL
   toEnum _ = error "Invalid Device Type value"



data CLCommandQueueProperty =
     CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
   | CL_QUEUE_PROFILING_ENABLE
   deriving (Show, Bounded, Eq, Ord)

instance Enum CLCommandQueueProperty where
   fromEnum CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = 1
   fromEnum CL_QUEUE_PROFILING_ENABLE              = 2

   toEnum 1 = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
   toEnum 2 = CL_QUEUE_PROFILING_ENABLE
   toEnum _ = error "Invalid Command Queue Property value"


data CLDeviceFPConfig =
     CL_FP_DENORM
   | CL_FP_INF_NAN
   | CL_FP_ROUND_TO_NEAREST
   | CL_FP_ROUND_TO_ZERO
   | CL_FP_ROUND_TO_INF
   | CL_FP_FMA
   | CL_FP_SOFT_FLOAT
   | CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT
   deriving (Show, Bounded, Eq, Ord)

instance Enum CLDeviceFPConfig where
   fromEnum CL_FP_DENORM                        = 1
   fromEnum CL_FP_INF_NAN                       = 2
   fromEnum CL_FP_ROUND_TO_NEAREST              = 4
   fromEnum CL_FP_ROUND_TO_ZERO                 = 8
   fromEnum CL_FP_ROUND_TO_INF                  = 16
   fromEnum CL_FP_FMA                           = 32
   fromEnum CL_FP_SOFT_FLOAT                    = 64
   fromEnum CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT = 128

   toEnum 1   = CL_FP_DENORM
   toEnum 2   = CL_FP_INF_NAN
   toEnum 4   = CL_FP_ROUND_TO_NEAREST
   toEnum 8   = CL_FP_ROUND_TO_ZERO
   toEnum 16  = CL_FP_ROUND_TO_INF
   toEnum 32  = CL_FP_FMA
   toEnum 64  = CL_FP_SOFT_FLOAT
   toEnum 128 = CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT
   toEnum _   = error "Invalid Device FP Config value"

data CLDeviceExecCapability =
     CL_EXEC_KERNEL
   | CL_EXEC_NATIVE_KERNEL
   deriving (Show, Bounded, Eq, Ord)

instance Enum CLDeviceExecCapability where
   fromEnum CL_EXEC_KERNEL        = 1
   fromEnum CL_EXEC_NATIVE_KERNEL = 2

   toEnum 1 = CL_EXEC_KERNEL
   toEnum 2 = CL_EXEC_NATIVE_KERNEL
   toEnum _ = error "Invalid Device Exec Capability value"

data CLDeviceMemCacheType =
     CL_NONE
   | CL_READ_ONLY_CACHE
   | CL_READ_WRITE_CACHE
   deriving (Show)

instance Enum CLDeviceMemCacheType where
   fromEnum CL_NONE              = 0x0
   fromEnum CL_READ_ONLY_CACHE   = 0x1
   fromEnum CL_READ_WRITE_CACHE  = 0x2

   toEnum 0x0 = CL_NONE
   toEnum 0x1 = CL_READ_ONLY_CACHE
   toEnum 0x2 = CL_READ_WRITE_CACHE
   toEnum _ = error "Invalid Device Mem Cache Type value"

data CLDeviceLocalMemType =
     CL_LOCAL
   | CL_GLOBAL
   deriving (Show)

instance Enum CLDeviceLocalMemType where
   fromEnum CL_LOCAL  = 0x1
   fromEnum CL_GLOBAL = 0x2

   toEnum 0x1 = CL_LOCAL
   toEnum 0x2 = CL_GLOBAL
   toEnum _ = error "Invalid Device Local Mem Type value"

data CLCommandType =
     CL_COMMAND_NDRANGE_KERNEL      
   | CL_COMMAND_TASK                
   | CL_COMMAND_NATIVE_KERNEL       
   | CL_COMMAND_READ_BUFFER         
   | CL_COMMAND_WRITE_BUFFER        
   | CL_COMMAND_COPY_BUFFER         
   | CL_COMMAND_READ_IMAGE          
   | CL_COMMAND_WRITE_IMAGE         
   | CL_COMMAND_COPY_IMAGE          
   | CL_COMMAND_COPY_IMAGE_TO_BUFFER
   | CL_COMMAND_COPY_BUFFER_TO_IMAGE
   | CL_COMMAND_MAP_BUFFER          
   | CL_COMMAND_MAP_IMAGE           
   | CL_COMMAND_UNMAP_MEM_OBJECT    
   | CL_COMMAND_MARKER              
   | CL_COMMAND_ACQUIRE_GL_OBJECTS  
   | CL_COMMAND_RELEASE_GL_OBJECTS  
   | CL_COMMAND_READ_BUFFER_RECT    
   | CL_COMMAND_WRITE_BUFFER_RECT   
   | CL_COMMAND_COPY_BUFFER_RECT    
   | CL_COMMAND_USER                
   | CL_COMMAND_BARRIER             
   | CL_COMMAND_MIGRATE_MEM_OBJECTS 
   | CL_COMMAND_FILL_BUFFER         
   | CL_COMMAND_FILL_IMAGE          
   deriving (Show)

instance Enum CLCommandType where
   fromEnum CL_COMMAND_NDRANGE_KERNEL       = 0x11F0
   fromEnum CL_COMMAND_TASK                 = 0x11F1
   fromEnum CL_COMMAND_NATIVE_KERNEL        = 0x11F2
   fromEnum CL_COMMAND_READ_BUFFER          = 0x11F3
   fromEnum CL_COMMAND_WRITE_BUFFER         = 0x11F4
   fromEnum CL_COMMAND_COPY_BUFFER          = 0x11F5
   fromEnum CL_COMMAND_READ_IMAGE           = 0x11F6
   fromEnum CL_COMMAND_WRITE_IMAGE          = 0x11F7
   fromEnum CL_COMMAND_COPY_IMAGE           = 0x11F8
   fromEnum CL_COMMAND_COPY_IMAGE_TO_BUFFER = 0x11F9
   fromEnum CL_COMMAND_COPY_BUFFER_TO_IMAGE = 0x11FA
   fromEnum CL_COMMAND_MAP_BUFFER           = 0x11FB
   fromEnum CL_COMMAND_MAP_IMAGE            = 0x11FC
   fromEnum CL_COMMAND_UNMAP_MEM_OBJECT     = 0x11FD
   fromEnum CL_COMMAND_MARKER               = 0x11FE
   fromEnum CL_COMMAND_ACQUIRE_GL_OBJECTS   = 0x11FF
   fromEnum CL_COMMAND_RELEASE_GL_OBJECTS   = 0x1200
   fromEnum CL_COMMAND_READ_BUFFER_RECT     = 0x1201
   fromEnum CL_COMMAND_WRITE_BUFFER_RECT    = 0x1202
   fromEnum CL_COMMAND_COPY_BUFFER_RECT     = 0x1203
   fromEnum CL_COMMAND_USER                 = 0x1204
   fromEnum CL_COMMAND_BARRIER              = 0x1205
   fromEnum CL_COMMAND_MIGRATE_MEM_OBJECTS  = 0x1206
   fromEnum CL_COMMAND_FILL_BUFFER          = 0x1207
   fromEnum CL_COMMAND_FILL_IMAGE           = 0x1208

   toEnum 0x11F0 = CL_COMMAND_NDRANGE_KERNEL
   toEnum 0x11F1 = CL_COMMAND_TASK
   toEnum 0x11F2 = CL_COMMAND_NATIVE_KERNEL
   toEnum 0x11F3 = CL_COMMAND_READ_BUFFER
   toEnum 0x11F4 = CL_COMMAND_WRITE_BUFFER
   toEnum 0x11F5 = CL_COMMAND_COPY_BUFFER
   toEnum 0x11F6 = CL_COMMAND_READ_IMAGE
   toEnum 0x11F7 = CL_COMMAND_WRITE_IMAGE
   toEnum 0x11F8 = CL_COMMAND_COPY_IMAGE
   toEnum 0x11F9 = CL_COMMAND_COPY_IMAGE_TO_BUFFER
   toEnum 0x11FA = CL_COMMAND_COPY_BUFFER_TO_IMAGE
   toEnum 0x11FB = CL_COMMAND_MAP_BUFFER
   toEnum 0x11FC = CL_COMMAND_MAP_IMAGE
   toEnum 0x11FD = CL_COMMAND_UNMAP_MEM_OBJECT
   toEnum 0x11FE = CL_COMMAND_MARKER
   toEnum 0x11FF = CL_COMMAND_ACQUIRE_GL_OBJECTS
   toEnum 0x1200 = CL_COMMAND_RELEASE_GL_OBJECTS
   toEnum 0x1201 = CL_COMMAND_READ_BUFFER_RECT
   toEnum 0x1202 = CL_COMMAND_WRITE_BUFFER_RECT
   toEnum 0x1203 = CL_COMMAND_COPY_BUFFER_RECT
   toEnum 0x1204 = CL_COMMAND_USER
   toEnum 0x1205 = CL_COMMAND_BARRIER
   toEnum 0x1206 = CL_COMMAND_MIGRATE_MEM_OBJECTS
   toEnum 0x1207 = CL_COMMAND_FILL_BUFFER
   toEnum 0x1208 = CL_COMMAND_FILL_IMAGE
   toEnum _ = error "Invalid Command Type value"

data CLCommandExecutionStatus =
     CL_COMPLETE
   | CL_RUNNING
   | CL_SUBMITTED
   | CL_QUEUED
   | CL_EXEC_ERROR
   deriving (Show)

instance Enum CLCommandExecutionStatus where
   fromEnum CL_COMPLETE    = 0x0
   fromEnum CL_RUNNING     = 0x1
   fromEnum CL_SUBMITTED   = 0x2
   fromEnum CL_QUEUED      = 0x3
   fromEnum CL_EXEC_ERROR  = -1

   toEnum 0x0 = CL_COMPLETE
   toEnum 0x1 = CL_RUNNING
   toEnum 0x2 = CL_SUBMITTED
   toEnum 0x3 = CL_QUEUED
   toEnum (-1)  = CL_EXEC_ERROR
   toEnum _ = error "Invalid Command Execution Status value"

data CLProfilingInfo =
     CL_PROFILING_COMMAND_QUEUED
   | CL_PROFILING_COMMAND_SUBMIT
   | CL_PROFILING_COMMAND_START
   | CL_PROFILING_COMMAND_END
   deriving (Show)

instance Enum CLProfilingInfo where
   fromEnum CL_PROFILING_COMMAND_QUEUED = 0x1280
   fromEnum CL_PROFILING_COMMAND_SUBMIT = 0x1281
   fromEnum CL_PROFILING_COMMAND_START  = 0x1282
   fromEnum CL_PROFILING_COMMAND_END    = 0x1283

   toEnum 0x1280 = CL_PROFILING_COMMAND_QUEUED
   toEnum 0x1281 = CL_PROFILING_COMMAND_SUBMIT
   toEnum 0x1282 = CL_PROFILING_COMMAND_START
   toEnum 0x1283 = CL_PROFILING_COMMAND_END
   toEnum _ = error "Invalid Profiling Info value"

data CLMemFlag =
     CL_MEM_READ_WRITE
   | CL_MEM_WRITE_ONLY
   | CL_MEM_READ_ONLY
   | CL_MEM_USE_HOST_PTR
   | CL_MEM_ALLOC_HOST_PTR
   | CL_MEM_COPY_HOST_PTR
   | CL_MEM_HOST_WRITE_ONLY
   | CL_MEM_HOST_READ_ONLY
   | CL_MEM_HOST_NO_ACCESS
   deriving (Show, Bounded, Eq, Ord)

instance Enum CLMemFlag where
   fromEnum CL_MEM_READ_WRITE      = 1
   fromEnum CL_MEM_WRITE_ONLY      = 2
   fromEnum CL_MEM_READ_ONLY       = 4
   fromEnum CL_MEM_USE_HOST_PTR    = 8
   fromEnum CL_MEM_ALLOC_HOST_PTR  = 16
   fromEnum CL_MEM_COPY_HOST_PTR   = 32
   fromEnum CL_MEM_HOST_WRITE_ONLY = 128
   fromEnum CL_MEM_HOST_READ_ONLY  = 256
   fromEnum CL_MEM_HOST_NO_ACCESS  = 512

   toEnum 1    = CL_MEM_READ_WRITE
   toEnum 2    = CL_MEM_WRITE_ONLY
   toEnum 4    = CL_MEM_READ_ONLY
   toEnum 8    = CL_MEM_USE_HOST_PTR
   toEnum 16   = CL_MEM_ALLOC_HOST_PTR
   toEnum 32   = CL_MEM_COPY_HOST_PTR
   toEnum 128  = CL_MEM_HOST_WRITE_ONLY
   toEnum 256  = CL_MEM_HOST_READ_ONLY
   toEnum 512  = CL_MEM_HOST_NO_ACCESS
   toEnum _ = error "Invalid Mem Flag value"


data CLMapFlag =
     CL_MAP_READ
   | CL_MAP_WRITE
   | CL_MAP_WRITE_INVALIDATE_REGION
   deriving (Show, Bounded, Eq, Ord)

instance Enum CLMapFlag where
   fromEnum CL_MAP_READ                     = 1
   fromEnum CL_MAP_WRITE                    = 2
   fromEnum CL_MAP_WRITE_INVALIDATE_REGION  = 4

   toEnum 1 = CL_MAP_READ
   toEnum 2 = CL_MAP_WRITE
   toEnum 4 = CL_MAP_WRITE_INVALIDATE_REGION
   toEnum _ = error "Invalid Map Flag value"


data CLMemObjectType =
     CL_MEM_OBJECT_BUFFER
   | CL_MEM_OBJECT_IMAGE2D
   | CL_MEM_OBJECT_IMAGE3D
   | CL_MEM_OBJECT_IMAGE2D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D
   | CL_MEM_OBJECT_IMAGE1D_ARRAY
   | CL_MEM_OBJECT_IMAGE1D_BUFFER
   deriving (Show)

instance Enum CLMemObjectType where
   fromEnum CL_MEM_OBJECT_BUFFER          = 0x10F0
   fromEnum CL_MEM_OBJECT_IMAGE2D         = 0x10F1
   fromEnum CL_MEM_OBJECT_IMAGE3D         = 0x10F2
   fromEnum CL_MEM_OBJECT_IMAGE2D_ARRAY   = 0x10F3
   fromEnum CL_MEM_OBJECT_IMAGE1D         = 0x10F4
   fromEnum CL_MEM_OBJECT_IMAGE1D_ARRAY   = 0x10F5
   fromEnum CL_MEM_OBJECT_IMAGE1D_BUFFER  = 0x10F6

   toEnum 0x10F0 = CL_MEM_OBJECT_BUFFER
   toEnum 0x10F1 = CL_MEM_OBJECT_IMAGE2D
   toEnum 0x10F2 = CL_MEM_OBJECT_IMAGE3D
   toEnum 0x10F3 = CL_MEM_OBJECT_IMAGE2D_ARRAY
   toEnum 0x10F4 = CL_MEM_OBJECT_IMAGE1D
   toEnum 0x10F5 = CL_MEM_OBJECT_IMAGE1D_ARRAY
   toEnum 0x10F6 = CL_MEM_OBJECT_IMAGE1D_BUFFER
   toEnum _ = error "Invalid Mem Object Type value"


data CLBuildStatus = 
     CL_BUILD_SUCCESS
   | CL_BUILD_NONE
   | CL_BUILD_ERROR
   | CL_BUILD_IN_PROGRESS
   deriving (Show)

instance Enum CLBuildStatus where
   fromEnum CL_BUILD_SUCCESS     = 0
   fromEnum CL_BUILD_NONE        = -1
   fromEnum CL_BUILD_ERROR       = -2
   fromEnum CL_BUILD_IN_PROGRESS = -3

   toEnum 0    = CL_BUILD_SUCCESS
   toEnum (-1) = CL_BUILD_NONE
   toEnum (-2) = CL_BUILD_ERROR
   toEnum (-3) = CL_BUILD_IN_PROGRESS
   toEnum _ = error "Invalid Build Status value"

data CLAddressingMode =
     CL_ADDRESS_NONE
   | CL_ADDRESS_CLAMP_TO_EDGE
   | CL_ADDRESS_CLAMP
   | CL_ADDRESS_REPEAT
   | CL_ADDRESS_MIRRORED_REPEAT
   deriving (Show)

instance Enum CLAddressingMode where
   fromEnum CL_ADDRESS_NONE            = 0x1130
   fromEnum CL_ADDRESS_CLAMP_TO_EDGE   = 0x1131
   fromEnum CL_ADDRESS_CLAMP           = 0x1132
   fromEnum CL_ADDRESS_REPEAT          = 0x1133
   fromEnum CL_ADDRESS_MIRRORED_REPEAT = 0x1134

   toEnum 0x1130 = CL_ADDRESS_NONE
   toEnum 0x1131 = CL_ADDRESS_CLAMP_TO_EDGE
   toEnum 0x1132 = CL_ADDRESS_CLAMP
   toEnum 0x1133 = CL_ADDRESS_REPEAT
   toEnum 0x1134 = CL_ADDRESS_MIRRORED_REPEAT
   toEnum _ = error "Invalid Addressing Mode value"

data CLFilterMode =
     CL_FILTER_NEAREST
   | CL_FILTER_LINEAR
   deriving (Show)

instance Enum CLFilterMode where
   fromEnum CL_FILTER_NEAREST = 0x1140
   fromEnum CL_FILTER_LINEAR  = 0x1141

   toEnum 0x1140 = CL_FILTER_NEAREST
   toEnum 0x1141 = CL_FILTER_LINEAR
   toEnum _ = error "Invalid Filter Mode value"


data CLChannelOrder =
     CL_R                         
   | CL_A                         
   | CL_RG                        
   | CL_RA                        
   | CL_RGB                       
   | CL_RGBA                      
   | CL_BGRA                      
   | CL_ARGB                      
   | CL_INTENSITY                 
   | CL_LUMINANCE                 
   | CL_Rx                        
   | CL_RGx                       
   | CL_RGBx                      
   | CL_DEPTH                     
   | CL_DEPTH_STENCIL             
   deriving (Show)

instance Enum CLChannelOrder where
   fromEnum CL_R              = 0x10B0
   fromEnum CL_A              = 0x10B1
   fromEnum CL_RG             = 0x10B2
   fromEnum CL_RA             = 0x10B3
   fromEnum CL_RGB            = 0x10B4
   fromEnum CL_RGBA           = 0x10B5
   fromEnum CL_BGRA           = 0x10B6
   fromEnum CL_ARGB           = 0x10B7
   fromEnum CL_INTENSITY      = 0x10B8
   fromEnum CL_LUMINANCE      = 0x10B9
   fromEnum CL_Rx             = 0x10BA
   fromEnum CL_RGx            = 0x10BB
   fromEnum CL_RGBx           = 0x10BC
   fromEnum CL_DEPTH          = 0x10BD
   fromEnum CL_DEPTH_STENCIL  = 0x10BE

   toEnum 0x10B0 = CL_R
   toEnum 0x10B1 = CL_A
   toEnum 0x10B2 = CL_RG
   toEnum 0x10B3 = CL_RA
   toEnum 0x10B4 = CL_RGB
   toEnum 0x10B5 = CL_RGBA
   toEnum 0x10B6 = CL_BGRA
   toEnum 0x10B7 = CL_ARGB
   toEnum 0x10B8 = CL_INTENSITY
   toEnum 0x10B9 = CL_LUMINANCE
   toEnum 0x10BA = CL_Rx
   toEnum 0x10BB = CL_RGx
   toEnum 0x10BC = CL_RGBx
   toEnum 0x10BD = CL_DEPTH
   toEnum 0x10BE = CL_DEPTH_STENCIL
   toEnum _ = error "Invalid Channel Order value"


data CLChannelType = 
     CL_SNORM_INT8                 
   | CL_SNORM_INT16                
   | CL_UNORM_INT8                 
   | CL_UNORM_INT16                
   | CL_UNORM_SHORT_565            
   | CL_UNORM_SHORT_555            
   | CL_UNORM_INT_101010           
   | CL_SIGNED_INT8                
   | CL_SIGNED_INT16               
   | CL_SIGNED_INT32               
   | CL_UNSIGNED_INT8              
   | CL_UNSIGNED_INT16             
   | CL_UNSIGNED_INT32             
   | CL_HALF_FLOAT                 
   | CL_FLOAT                      
   | CL_UNORM_INT24                
   deriving (Show)

instance Enum CLChannelType where
   fromEnum CL_SNORM_INT8       = 0x10D0
   fromEnum CL_SNORM_INT16      = 0x10D1
   fromEnum CL_UNORM_INT8       = 0x10D2
   fromEnum CL_UNORM_INT16      = 0x10D3
   fromEnum CL_UNORM_SHORT_565  = 0x10D4
   fromEnum CL_UNORM_SHORT_555  = 0x10D5
   fromEnum CL_UNORM_INT_101010 = 0x10D6
   fromEnum CL_SIGNED_INT8      = 0x10D7
   fromEnum CL_SIGNED_INT16     = 0x10D8
   fromEnum CL_SIGNED_INT32     = 0x10D9
   fromEnum CL_UNSIGNED_INT8    = 0x10DA
   fromEnum CL_UNSIGNED_INT16   = 0x10DB
   fromEnum CL_UNSIGNED_INT32   = 0x10DC
   fromEnum CL_HALF_FLOAT       = 0x10DD
   fromEnum CL_FLOAT            = 0x10DE
   fromEnum CL_UNORM_INT24      = 0x10DF

   toEnum 0x10D0 = CL_SNORM_INT8
   toEnum 0x10D1 = CL_SNORM_INT16
   toEnum 0x10D2 = CL_UNORM_INT8
   toEnum 0x10D3 = CL_UNORM_INT16
   toEnum 0x10D4 = CL_UNORM_SHORT_565
   toEnum 0x10D5 = CL_UNORM_SHORT_555
   toEnum 0x10D6 = CL_UNORM_INT_101010
   toEnum 0x10D7 = CL_SIGNED_INT8
   toEnum 0x10D8 = CL_SIGNED_INT16
   toEnum 0x10D9 = CL_SIGNED_INT32
   toEnum 0x10DA = CL_UNSIGNED_INT8
   toEnum 0x10DB = CL_UNSIGNED_INT16
   toEnum 0x10DC = CL_UNSIGNED_INT32
   toEnum 0x10DD = CL_HALF_FLOAT
   toEnum 0x10DE = CL_FLOAT
   toEnum 0x10DF = CL_UNORM_INT24
   toEnum _ = error "Invalid Channel Type value"

data CLImageFormat = CLImageFormat
                     { image_channel_order :: ! CLChannelOrder
                     , image_channel_data_type :: ! CLChannelType }
                     deriving( Show )

type CLImageFormat_p = Ptr CLImageFormat

instance Storable CLImageFormat where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = 64
  peek p =
    CLImageFormat <$> fmap getEnumCL (peekByteOff p 0 :: IO Word32)
                  <*> fmap getEnumCL (peekByteOff p 4 :: IO Word32)
  poke p (CLImageFormat a b) = do
      pokeByteOff p 0 (getCLValue a :: Word32)
      pokeByteOff p 4 (getCLValue b :: Word32)


type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import CALLCONV "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)

type NativeKernelCallback = Ptr () -> IO ()
foreign import CALLCONV "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)

type BuildCallback = CLProgram -> Ptr () -> IO ()

-- -----------------------------------------------------------------------------

withMaybeArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withMaybeArray [] = ($ nullPtr)
withMaybeArray xs = withArray xs

getCLValue :: (Enum a, Integral b) => a -> b
getCLValue = fromIntegral . fromEnum

getEnumCL :: (Integral a, Enum b) => a -> b
getEnumCL = toEnum . fromIntegral 

getCommandExecutionStatus :: CLint -> CLCommandExecutionStatus
getCommandExecutionStatus n 
  | n < 0 = CL_EXEC_ERROR
  | otherwise = getEnumCL $ n
                
-- -----------------------------------------------------------------------------
binaryFlags :: (Ord b, Enum b, Bounded b) => b -> [b]
binaryFlags m = map toEnum . takeWhile (<= (fromEnum m)) $ [1 `shiftL` n | n <- [0..]]
  
testMask :: Bits b => b -> b -> Bool
testMask mask v = (v .&. mask) == v

bitmaskFromFlags :: (Enum a, Num b, Bits b) => [a] -> b
bitmaskFromFlags = foldl' (.|.) 0 . map (fromIntegral . fromEnum)

bitmaskToFlags :: (Enum a, Num b, Bits b) => [a] -> b -> [a]
bitmaskToFlags xs mask = filter (testMask mask . fromIntegral . fromEnum) xs

bitmaskToDeviceTypes :: CLDeviceType_ -> [CLDeviceType]
bitmaskToDeviceTypes =
	bitmaskToFlags 
		[CL_DEVICE_TYPE_CPU
		,CL_DEVICE_TYPE_GPU
		,CL_DEVICE_TYPE_ACCELERATOR
		,CL_DEVICE_TYPE_DEFAULT
		,CL_DEVICE_TYPE_ALL
		]

bitmaskToCommandQueueProperties :: CLCommandQueueProperty_ -> [CLCommandQueueProperty]
bitmaskToCommandQueueProperties = bitmaskToFlags (binaryFlags maxBound)
      
bitmaskToFPConfig :: CLDeviceFPConfig_ -> [CLDeviceFPConfig]
bitmaskToFPConfig = bitmaskToFlags (binaryFlags maxBound)

bitmaskToExecCapability :: CLDeviceExecCapability_ -> [CLDeviceExecCapability]
bitmaskToExecCapability = bitmaskToFlags (binaryFlags maxBound)

bitmaskToMemFlags :: CLMemFlags_ -> [CLMemFlag]
bitmaskToMemFlags = bitmaskToFlags (binaryFlags maxBound)

-- -----------------------------------------------------------------------------

data CLCommandQueueInfo = 
     CL_QUEUE_CONTEXT 
   | CL_QUEUE_DEVICE
   | CL_QUEUE_REFERENCE_COUNT
   | CL_QUEUE_PROPERTIES

instance Enum CLCommandQueueInfo where
   fromEnum CL_QUEUE_CONTEXT           = 0x1090
   fromEnum CL_QUEUE_DEVICE            = 0x1091
   fromEnum CL_QUEUE_REFERENCE_COUNT   = 0x1092
   fromEnum CL_QUEUE_PROPERTIES        = 0x1093
   toEnum 0x1090 = CL_QUEUE_CONTEXT
   toEnum 0x1091 = CL_QUEUE_DEVICE
   toEnum 0x1092 = CL_QUEUE_REFERENCE_COUNT
   toEnum 0x1093 = CL_QUEUE_PROPERTIES
   toEnum _ = error "Invalid CLCommandQueueInfo value"

