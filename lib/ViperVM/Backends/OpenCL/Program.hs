{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Backends.OpenCL.Program(  
  -- * Types
  Program(..), CLProgram, CLBuildStatus(..), CLKernel,
  -- * Program Functions
  clCreateProgramWithSource, clCreateProgramWithBinary, clRetainProgram, 
  clReleaseProgram, clUnloadCompiler, clBuildProgram, 
  clGetProgramReferenceCount, clGetProgramContext, clGetProgramNumDevices, 
  clGetProgramDevices, clGetProgramSource, clGetProgramBinarySizes, 
  clGetProgramBinaries, clGetProgramBuildStatus, clGetProgramBuildOptions, 
  clGetProgramBuildLog,
  -- * Kernel Functions
  clCreateKernel, clCreateKernelsInProgram, clRetainKernel, clReleaseKernel, 
  clSetKernelArg, clSetKernelArgSto, clGetKernelFunctionName, clGetKernelNumArgs, 
  clGetKernelReferenceCount, clGetKernelContext, clGetKernelProgram, 
  clGetKernelWorkGroupSize, clGetKernelCompileWorkGroupSize, 
  clGetKernelLocalMemSize, programForProcessor, initProgram
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( zipWithM, forM )
import Control.Applicative ( (<$>) )
import Foreign
import Foreign.C.Types
import Foreign.C.String( CString, withCString, peekCString )
import ViperVM.STM.TSet (TSet)
import qualified ViperVM.STM.TSet as TSet
import Data.Maybe (listToMaybe)
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Processor
import ViperVM.Backends.OpenCL.Types(
  CLint, CLuint, CLulong, CLProgram, CLContext, CLKernel, CLDeviceID, CLError,
  CLProgramInfo_, CLBuildStatus(..), CLBuildStatus_, 
  CLKernelInfo_, wrapCheckSuccess, 
  whenSuccess, wrapPError, wrapGetInfo, getCLValue, getEnumCL )
import Control.Concurrent.STM
import Control.Monad (filterM)


-- | Multiplex OpenCL programs from different platforms
data Program = Program {
   programSource :: String,
   programPeers :: TSet ProgramPeer
}

data ProgramPeer = ProgramPeer OpenCLLibrary CLProgram

instance Eq Program where
   (==) p1 p2 = programSource p1 == programSource p2

instance Ord Program where
   compare p1 p2 = compare (programSource p1) (programSource p2)


initProgram :: String -> IO Program
initProgram src = Program src <$> atomically TSet.empty

programForProcessor :: Program -> Processor -> IO (Maybe CLProgram)
programForProcessor prog proc = do
   let dev = procDevice proc
       f (ProgramPeer lib p) = elem dev <$> clGetProgramDevices lib p
       g (ProgramPeer _ p) = p
   peers <- atomically (TSet.elems (programPeers prog))
   listToMaybe . fmap g <$> filterM f peers
   

clCreateProgramWithSource :: OpenCLLibrary -> CLContext -> String -> IO CLProgram
clCreateProgramWithSource lib ctx source =
  withCString source $ \cSource ->
  withArray [cSource] $ \sourcesP ->
  wrapPError (rawClCreateProgramWithSource lib ctx 1 sourcesP nullPtr)

clCreateProgramWithBinary :: OpenCLLibrary -> CLContext -> [CLDeviceID] -> [[Word8]] 
                             -> IO (CLProgram, [CLError])
clCreateProgramWithBinary lib ctx devs bins = wrapPError $ \perr ->
  withArray devs $ \pdevs ->
    withArray lbins $ \plbins -> do
      buffs <- forM bins $ \bs -> do
        buff <- mallocArray (length bs) :: IO (Ptr Word8)
        pokeArray buff bs
        return buff

      ret <- withArray buffs $ \(pbuffs :: Ptr (Ptr Word8)) -> do
        allocaArray ndevs $ \(perrs :: Ptr CLint) -> do
          prog <- rawClCreateProgramWithBinary lib ctx (fromIntegral ndevs) pdevs plbins pbuffs perrs perr
          errs <- peekArray ndevs perrs
          return (prog, map getEnumCL errs)

      mapM_ free buffs
      return ret
    
    where
      lbins = map (fromIntegral . length) bins :: [CSize]
      ndevs = length devs

-- | Increments the program reference count. 'clRetainProgram' returns 'True' if 
-- the function is executed successfully. It returns 'False' if program is not a 
-- valid program object.
clRetainProgram :: OpenCLLibrary -> CLProgram -> IO Bool
clRetainProgram lib prg = wrapCheckSuccess $ rawClRetainProgram lib prg

-- | Decrements the program reference count. The program object is deleted after 
-- all kernel objects associated with program have been deleted and the program 
-- reference count becomes zero. 'clReleseProgram' returns 'True' if 
-- the function is executed successfully. It returns 'False' if program is not a 
-- valid program object.
clReleaseProgram :: OpenCLLibrary -> CLProgram -> IO Bool
clReleaseProgram lib prg = wrapCheckSuccess $ rawClReleaseProgram lib prg

-- | Allows the implementation to release the resources allocated by the OpenCL
-- compiler. This is a hint from the application and does not guarantee that the
-- compiler will not be used in the future or that the compiler will actually be
-- unloaded by the implementation. Calls to 'clBuildProgram' after
-- 'clUnloadCompiler' will reload the compiler, if necessary, to build the
-- appropriate program executable.
clUnloadCompiler :: OpenCLLibrary -> IO ()
clUnloadCompiler lib = rawClUnloadCompiler lib >> return ()

clBuildProgram :: OpenCLLibrary -> CLProgram -> [CLDeviceID] -> String -> IO CLError
clBuildProgram lib prg devs opts = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  withCString opts $ \copts -> do
    getEnumCL <$> rawClBuildProgram lib prg cndevs pdevs copts nullFunPtr nullPtr
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

data CLProgramInfo =
     CL_PROGRAM_REFERENCE_COUNT
   | CL_PROGRAM_CONTEXT
   | CL_PROGRAM_NUM_DEVICES
   | CL_PROGRAM_DEVICES
   | CL_PROGRAM_SOURCE
   | CL_PROGRAM_BINARY_SIZES
   | CL_PROGRAM_BINARIES
   | CL_PROGRAM_NUM_KERNELS
   | CL_PROGRAM_KERNEL_NAMES

instance Enum CLProgramInfo where
   fromEnum CL_PROGRAM_REFERENCE_COUNT = 0x1160
   fromEnum CL_PROGRAM_CONTEXT         = 0x1161
   fromEnum CL_PROGRAM_NUM_DEVICES     = 0x1162
   fromEnum CL_PROGRAM_DEVICES         = 0x1163
   fromEnum CL_PROGRAM_SOURCE          = 0x1164
   fromEnum CL_PROGRAM_BINARY_SIZES    = 0x1165
   fromEnum CL_PROGRAM_BINARIES        = 0x1166
   fromEnum CL_PROGRAM_NUM_KERNELS     = 0x1167
   fromEnum CL_PROGRAM_KERNEL_NAMES    = 0x1168

   toEnum 0x1160 = CL_PROGRAM_REFERENCE_COUNT
   toEnum 0x1161 = CL_PROGRAM_CONTEXT
   toEnum 0x1162 = CL_PROGRAM_NUM_DEVICES
   toEnum 0x1163 = CL_PROGRAM_DEVICES
   toEnum 0x1164 = CL_PROGRAM_SOURCE
   toEnum 0x1165 = CL_PROGRAM_BINARY_SIZES
   toEnum 0x1166 = CL_PROGRAM_BINARIES
   toEnum 0x1167 = CL_PROGRAM_NUM_KERNELS
   toEnum 0x1168 = CL_PROGRAM_KERNEL_NAMES
   toEnum _ = error "Invalid Program Info value"

getProgramInfoSize :: OpenCLLibrary -> CLProgram -> CLProgramInfo_ -> IO CSize
getProgramInfoSize lib prg infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (rawClGetProgramInfo lib prg infoid 0 nullPtr value_size)
    $ peek value_size
  
-- | Return the program reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetProgramInfo with
-- 'CL_PROGRAM_REFERENCE_COUNT'.
clGetProgramReferenceCount :: OpenCLLibrary -> CLProgram -> IO CLuint
clGetProgramReferenceCount lib prg =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetProgramInfo lib prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the context specified when the program object is created.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_CONTEXT'.
clGetProgramContext :: OpenCLLibrary -> CLProgram -> IO CLContext
clGetProgramContext lib prg =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        rawClGetProgramInfo lib prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the number of devices associated with program.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_NUM_DEVICES'.
clGetProgramNumDevices :: OpenCLLibrary -> CLProgram -> IO CLuint
clGetProgramNumDevices lib prg =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetProgramInfo lib prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_NUM_DEVICES
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the list of devices associated with the program object. This can be
-- the devices associated with context on which the program object has been
-- created or can be a subset of devices that are specified when a progam object
-- is created using 'clCreateProgramWithBinary'.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_DEVICES'.
clGetProgramDevices :: OpenCLLibrary -> CLProgram -> IO [CLDeviceID]
clGetProgramDevices lib prg = do
  size <- getProgramInfoSize lib prg infoid
  allocaArray (numElems size) $ \(buff :: Ptr CLDeviceID) -> do
    whenSuccess (rawClGetProgramInfo lib prg infoid size (castPtr buff) nullPtr)
      $ peekArray (numElems size) buff
    where 
      infoid = getCLValue CL_PROGRAM_DEVICES
      numElems s = (fromIntegral s) `div` elemSize
      elemSize = sizeOf (nullPtr::CLDeviceID)

-- | Return the program source code specified by
-- 'clCreateProgramWithSource'. The source string returned is a concatenation of
-- all source strings specified to 'clCreateProgramWithSource' with a null
-- terminator. The concatenation strips any nulls in the original source
-- strings. The actual number of characters that represents the program source
-- code including the null terminator is returned in param_value_size_ret.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_SOURCE'.
clGetProgramSource :: OpenCLLibrary -> CLProgram -> IO String
clGetProgramSource lib prg = do
  n <- getProgramInfoSize lib prg infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (rawClGetProgramInfo lib prg infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_SOURCE
  
-- | Returns an array that contains the size in bytes of the program binary for
-- each device associated with program. The size of the array is the number of
-- devices associated with program. If a binary is not available for a
-- device(s), a size of zero is returned.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_BINARY_SIZES'.
clGetProgramBinarySizes :: OpenCLLibrary -> CLProgram -> IO [CSize]
clGetProgramBinarySizes lib prg = do
  size <- getProgramInfoSize lib prg infoid
  allocaArray (numElems size) $ \(buff :: Ptr CSize) -> do
    whenSuccess (rawClGetProgramInfo lib prg infoid size (castPtr buff) nullPtr)
      $ peekArray (numElems size) buff
    where 
      infoid = getCLValue CL_PROGRAM_BINARY_SIZES
      numElems s = (fromIntegral s) `div` elemSize
      elemSize = sizeOf (0::CSize)

{-| Return the program binaries for all devices associated with program. For
each device in program, the binary returned can be the binary specified for the
device when program is created with 'clCreateProgramWithBinary' or it can be the
executable binary generated by 'clBuildProgram'. If program is created with
'clCreateProgramWithSource', the binary returned is the binary generated by
'clBuildProgram'. The bits returned can be an implementation-specific
intermediate representation (a.k.a. IR) or device specific executable bits or
both. The decision on which information is returned in the binary is up to the
OpenCL implementation.

To find out which device the program binary in the array refers to, use the
'clGetProgramDevices' query to get the list of devices. There is a one-to-one
correspondence between the array of data returned by 'clGetProgramBinaries' and
array of devices returned by 'clGetProgramDevices'.  

This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_BINARIES'.
-}
clGetProgramBinaries :: OpenCLLibrary -> CLProgram -> IO [[Word8]]
clGetProgramBinaries lib prg = do
  sizes <- clGetProgramBinarySizes lib prg
  let numElems = length sizes
      size = fromIntegral $ numElems * elemSize
  buffers <- (mapM (mallocArray.fromIntegral) sizes) :: IO [Ptr Word8]
  ret <- withArray buffers $ \(parray :: Ptr (Ptr Word8)) -> do
    whenSuccess (rawClGetProgramInfo lib prg infoid size (castPtr parray) nullPtr)
      $ zipWithM peekArray (map fromIntegral sizes) buffers
  mapM_ free buffers
  return ret
    where 
      infoid = getCLValue CL_PROGRAM_BINARIES
      elemSize = sizeOf (nullPtr::Ptr Word8)


data CLProgramBuildInfo =
     CL_PROGRAM_BUILD_STATUS
   | CL_PROGRAM_BUILD_OPTIONS
   | CL_PROGRAM_BUILD_LOG
   | CL_PROGRAM_BINARY_TYPE

instance Enum CLProgramBuildInfo where
   fromEnum CL_PROGRAM_BUILD_STATUS  = 0x1181
   fromEnum CL_PROGRAM_BUILD_OPTIONS = 0x1182
   fromEnum CL_PROGRAM_BUILD_LOG     = 0x1183
   fromEnum CL_PROGRAM_BINARY_TYPE   = 0x1184

   toEnum 0x1181 = CL_PROGRAM_BUILD_STATUS
   toEnum 0x1182 = CL_PROGRAM_BUILD_OPTIONS
   toEnum 0x1183 = CL_PROGRAM_BUILD_LOG
   toEnum 0x1184 = CL_PROGRAM_BINARY_TYPE
   toEnum _ = error "Invalid Program Build Info value"

getProgramBuildInfoSize :: OpenCLLibrary -> CLProgram -> CLDeviceID -> CLProgramInfo_ -> IO CSize
getProgramBuildInfoSize lib prg device infoid = alloca $ \(val :: Ptr CSize) -> do
  whenSuccess (rawClGetProgramBuildInfo lib prg device infoid 0 nullPtr val)
    $ peek val
  
-- | Returns the build status of program for a specific device as given by
-- device.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_STATUS'.
clGetProgramBuildStatus :: OpenCLLibrary -> CLProgram -> CLDeviceID -> IO CLBuildStatus
clGetProgramBuildStatus lib prg device =
    wrapGetInfo (\(dat :: Ptr CLBuildStatus_) ->
        rawClGetProgramBuildInfo lib prg device infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_STATUS
      size = fromIntegral $ sizeOf (0::CLBuildStatus_)

-- | Return the build options specified by the options argument in
-- clBuildProgram for device. If build status of program for device is
-- 'CL_BUILD_NONE', an empty string is returned.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_OPTIONS'.
clGetProgramBuildOptions :: OpenCLLibrary -> CLProgram -> CLDeviceID -> IO String
clGetProgramBuildOptions lib prg device = do
  n <- getProgramBuildInfoSize lib prg device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (rawClGetProgramBuildInfo lib prg device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_OPTIONS
  
-- | Return the build log when 'clBuildProgram' was called for device. If build
-- status of program for device is 'CL_BUILD_NONE', an empty string is returned.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_LOG'.
clGetProgramBuildLog :: OpenCLLibrary -> CLProgram -> CLDeviceID -> IO String
clGetProgramBuildLog lib prg device = do
  n <- getProgramBuildInfoSize lib prg device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (rawClGetProgramBuildInfo lib prg device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_LOG
  
clCreateKernel :: OpenCLLibrary -> CLProgram -> String -> IO CLKernel
clCreateKernel lib prg name = withCString name $ \cname -> wrapPError $ \perr -> do
  rawClCreateKernel lib prg cname perr

clCreateKernelsInProgram :: OpenCLLibrary -> CLProgram -> IO [CLKernel]
clCreateKernelsInProgram lib prg = do
  n <- alloca $ \pn -> do
    whenSuccess (rawClCreateKernelsInProgram lib prg 0 nullPtr pn)
      $ peek pn  
  allocaArray (fromIntegral n) $ \pks -> do
    whenSuccess (rawClCreateKernelsInProgram lib prg n pks nullPtr)
      $ peekArray (fromIntegral n) pks

-- | Increments the program program reference count. 'clRetainKernel' returns
-- 'True' if the function is executed successfully. 'clCreateKernel' or
-- 'clCreateKernelsInProgram' do an implicit retain.
clRetainKernel :: OpenCLLibrary -> CLKernel -> IO Bool
clRetainKernel lib krn = wrapCheckSuccess $ rawClRetainKernel lib krn

-- | Decrements the kernel reference count. The kernel object is deleted once
-- the number of instances that are retained to kernel become zero and the
-- kernel object is no longer needed by any enqueued commands that use
-- kernel. 'clReleaseKernel' returns 'True' if the function is executed
-- successfully.
clReleaseKernel :: OpenCLLibrary -> CLKernel -> IO Bool
clReleaseKernel lib krn = wrapCheckSuccess $ rawClReleaseKernel lib krn

clSetKernelArg :: Integral a => OpenCLLibrary -> CLKernel -> CLuint -> a -> Ptr b -> IO ()
clSetKernelArg lib krn idx sz pval = do
  whenSuccess (rawClSetKernelArg lib krn idx (fromIntegral sz) (castPtr pval))
    $ return ()

-- | Wrap function of `clSetKernelArg` with Storable data.
clSetKernelArgSto :: Storable a => OpenCLLibrary -> CLKernel -> CLuint -> a -> IO ()
clSetKernelArgSto lib krn idx val = with val $ \pval -> do
  whenSuccess (rawClSetKernelArg lib krn idx (fromIntegral . sizeOf $ val) (castPtr pval))
    $ return ()

data CLKernelInfo =
     CL_KERNEL_FUNCTION_NAME
   | CL_KERNEL_NUM_ARGS
   | CL_KERNEL_REFERENCE_COUNT
   | CL_KERNEL_CONTEXT
   | CL_KERNEL_PROGRAM
   | CL_KERNEL_ATTRIBUTES

instance Enum CLKernelInfo where
   fromEnum CL_KERNEL_FUNCTION_NAME   = 0x1190
   fromEnum CL_KERNEL_NUM_ARGS        = 0x1191
   fromEnum CL_KERNEL_REFERENCE_COUNT = 0x1192
   fromEnum CL_KERNEL_CONTEXT         = 0x1193
   fromEnum CL_KERNEL_PROGRAM         = 0x1194
   fromEnum CL_KERNEL_ATTRIBUTES      = 0x1195

   toEnum 0x1190 = CL_KERNEL_FUNCTION_NAME
   toEnum 0x1191 = CL_KERNEL_NUM_ARGS
   toEnum 0x1192 = CL_KERNEL_REFERENCE_COUNT
   toEnum 0x1193 = CL_KERNEL_CONTEXT
   toEnum 0x1194 = CL_KERNEL_PROGRAM
   toEnum 0x1195 = CL_KERNEL_ATTRIBUTES
   toEnum _ = error "Invalid Kernel Info value"

getKernelInfoSize :: OpenCLLibrary -> CLKernel -> CLKernelInfo_ -> IO CSize
getKernelInfoSize lib krn infoid = alloca $ \(val :: Ptr CSize) -> do
  whenSuccess (rawClGetKernelInfo lib krn infoid 0 nullPtr val)
    $ peek val
  
-- | Return the kernel function name.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_FUNCTION_NAME'.
clGetKernelFunctionName :: OpenCLLibrary -> CLKernel -> IO String
clGetKernelFunctionName lib krn = do
  n <- getKernelInfoSize lib krn infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (rawClGetKernelInfo lib krn infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_KERNEL_FUNCTION_NAME

-- | Return the number of arguments to kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_NUM_ARGS'.
clGetKernelNumArgs :: OpenCLLibrary -> CLKernel -> IO CLuint
clGetKernelNumArgs lib krn =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetKernelInfo lib krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_NUM_ARGS
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the kernel reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetKernelInfo with
-- 'CL_KERNEL_REFERENCE_COUNT'.
clGetKernelReferenceCount :: OpenCLLibrary -> CLKernel -> IO CLuint
clGetKernelReferenceCount lib krn =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetKernelInfo lib krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the context associated with kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_CONTEXT'.
clGetKernelContext :: OpenCLLibrary -> CLKernel -> IO CLContext
clGetKernelContext lib krn =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        rawClGetKernelInfo lib krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the program object associated with kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_PROGRAM'.
clGetKernelProgram :: OpenCLLibrary -> CLKernel -> IO CLProgram
clGetKernelProgram lib krn =
    wrapGetInfo (\(dat :: Ptr CLProgram) ->
        rawClGetKernelInfo lib krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_PROGRAM
      size = fromIntegral $ sizeOf (nullPtr::CLProgram)

data CLKernelGroupInfo =
     CL_KERNEL_WORK_GROUP_SIZE
   | CL_KERNEL_COMPILE_WORK_GROUP_SIZE
   | CL_KERNEL_LOCAL_MEM_SIZE
   | CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE
   | CL_KERNEL_PRIVATE_MEM_SIZE
   | CL_KERNEL_GLOBAL_WORK_SIZE

instance Enum CLKernelGroupInfo where
   fromEnum CL_KERNEL_WORK_GROUP_SIZE                    = 0x11B0
   fromEnum CL_KERNEL_COMPILE_WORK_GROUP_SIZE            = 0x11B1
   fromEnum CL_KERNEL_LOCAL_MEM_SIZE                     = 0x11B2
   fromEnum CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE = 0x11B3
   fromEnum CL_KERNEL_PRIVATE_MEM_SIZE                   = 0x11B4
   fromEnum CL_KERNEL_GLOBAL_WORK_SIZE                   = 0x11B5

   toEnum 0x11B0 = CL_KERNEL_WORK_GROUP_SIZE
   toEnum 0x11B1 = CL_KERNEL_COMPILE_WORK_GROUP_SIZE
   toEnum 0x11B2 = CL_KERNEL_LOCAL_MEM_SIZE
   toEnum 0x11B3 = CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE
   toEnum 0x11B4 = CL_KERNEL_PRIVATE_MEM_SIZE
   toEnum 0x11B5 = CL_KERNEL_GLOBAL_WORK_SIZE
   toEnum _ = error "Invalid Kernel Work Group Info value"


-- | This provides a mechanism for the application to query the work-group size
-- that can be used to execute a kernel on a specific device given by
-- device. The OpenCL implementation uses the resource requirements of the
-- kernel (register usage etc.) to determine what this work-group size should
-- be.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_WORK_GROUP_SIZE'.
clGetKernelWorkGroupSize :: OpenCLLibrary -> CLKernel -> CLDeviceID -> IO CSize
clGetKernelWorkGroupSize lib krn device =
    wrapGetInfo (\(dat :: Ptr CSize) ->
        rawClGetKernelWorkGroupInfo lib krn device infoid size (castPtr dat)) id
    where
      infoid = getCLValue CL_KERNEL_WORK_GROUP_SIZE
      size = fromIntegral $ sizeOf (0::CSize)

-- | Returns the work-group size specified by the __attribute__((reqd_work_gr
-- oup_size(X, Y, Z))) qualifier. See Function Qualifiers. If the work-group
-- size is not specified using the above attribute qualifier (0, 0, 0) is
-- returned.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_COMPILE_WORK_GROUP_SIZE'.
clGetKernelCompileWorkGroupSize :: OpenCLLibrary -> CLKernel -> CLDeviceID -> IO [CSize]
clGetKernelCompileWorkGroupSize lib krn device = do
  allocaArray num $ \(buff :: Ptr CSize) -> do
    whenSuccess (rawClGetKernelWorkGroupInfo lib krn device infoid size (castPtr buff) nullPtr)
      $ peekArray num buff
    where 
      infoid = getCLValue CL_KERNEL_COMPILE_WORK_GROUP_SIZE
      num = 3
      elemSize = fromIntegral $ sizeOf (0::CSize)
      size = fromIntegral $ num * elemSize


-- | Returns the amount of local memory in bytes being used by a kernel. This
-- includes local memory that may be needed by an implementation to execute the
-- kernel, variables declared inside the kernel with the __local address
-- qualifier and local memory to be allocated for arguments to the kernel
-- declared as pointers with the __local address qualifier and whose size is
-- specified with 'clSetKernelArg'.
--
-- If the local memory size, for any pointer argument to the kernel declared
-- with the __local address qualifier, is not specified, its size is assumed to
-- be 0.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_LOCAL_MEM_SIZE'.
clGetKernelLocalMemSize :: OpenCLLibrary -> CLKernel -> CLDeviceID -> IO CLulong
clGetKernelLocalMemSize lib krn device =
    wrapGetInfo (\(dat :: Ptr CLulong) ->
        rawClGetKernelWorkGroupInfo lib krn device infoid size (castPtr dat)) id
    where
      infoid = getCLValue CL_KERNEL_LOCAL_MEM_SIZE
      size = fromIntegral $ sizeOf (0::CLulong)
-- -----------------------------------------------------------------------------
