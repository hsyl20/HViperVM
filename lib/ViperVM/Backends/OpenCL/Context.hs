{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Backends.OpenCL.Context(
  -- * Types
  CLContext, CLContextProperty(..),
  -- * Context Functions
  clCreateContext, clCreateContextFromType, clRetainContext, clReleaseContext,
  clGetContextReferenceCount, clGetContextDevices, clGetContextProperties )
    where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray, 
  ptrToIntPtr, intPtrToPtr, withArray )
import Foreign.C.Types( CSize(..) )
import Foreign.C.String( peekCString )
import Foreign.Storable( sizeOf )
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Types( 
  CLuint, CLContextInfo_, CLContextProperty_, CLDeviceID, 
  CLContext, CLDeviceType, CLPlatformID, bitmaskFromFlags, getCLValue, getEnumCL,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo, wrapContextCallback, ContextCallback )

data CLContextProperties = 
     CL_CONTEXT_PLATFORM_
   | CL_GL_CONTEXT_KHR_
   | CL_EGL_DISPLAY_KHR_
   | CL_GLX_DISPLAY_KHR_
   | CL_WGL_HDC_KHR_
   | CL_CGL_SHAREGROUP_KHR_

instance Enum CLContextProperties where
   fromEnum CL_CONTEXT_PLATFORM_    = 0x1084
   fromEnum CL_GL_CONTEXT_KHR_      = 0x2008
   fromEnum CL_EGL_DISPLAY_KHR_     = 0x2009
   fromEnum CL_GLX_DISPLAY_KHR_     = 0x200A
   fromEnum CL_WGL_HDC_KHR_         = 0x200B
   fromEnum CL_CGL_SHAREGROUP_KHR_  = 0x200C

   toEnum 0x1084 = CL_CONTEXT_PLATFORM_
   toEnum 0x2008 = CL_GL_CONTEXT_KHR_
   toEnum 0x2009 = CL_EGL_DISPLAY_KHR_
   toEnum 0x200A = CL_GLX_DISPLAY_KHR_
   toEnum 0x200B = CL_WGL_HDC_KHR_
   toEnum 0x200C = CL_CGL_SHAREGROUP_KHR_
   toEnum _ = error "Invalid Context Property value"

-- | Specifies a context property name and its corresponding value.
data CLContextProperty = CL_CONTEXT_PLATFORM CLPlatformID 
                       | CL_CGL_SHAREGROUP_KHR (Ptr ())
                       | CL_GL_CONTEXT_KHR (Ptr ())
                       | CL_EGL_DISPLAY_KHR (Ptr ())
                       | CL_GLX_DISPLAY_KHR (Ptr ())
                       | CL_WGL_HDC_KHR (Ptr ())
                       deriving( Show )

packProperty :: CLContextProperty -> [CLContextProperty_]
packProperty (CL_CONTEXT_PLATFORM pid)   = [ getCLValue CL_CONTEXT_PLATFORM_
                                           , fromIntegral . ptrToIntPtr $ pid ]
packProperty (CL_CGL_SHAREGROUP_KHR ptr) = [ getCLValue CL_CGL_SHAREGROUP_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_GL_CONTEXT_KHR ptr)     = [ getCLValue CL_GL_CONTEXT_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_EGL_DISPLAY_KHR ptr)    = [ getCLValue CL_EGL_DISPLAY_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_GLX_DISPLAY_KHR ptr)    = [ getCLValue CL_GLX_DISPLAY_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_WGL_HDC_KHR ptr)        = [ getCLValue CL_WGL_HDC_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]

packContextProperties :: [CLContextProperty] -> [CLContextProperty_]
packContextProperties [] = [0]
packContextProperties (x:xs) = packProperty x ++ packContextProperties xs

unpackContextProperties :: [CLContextProperty_] -> [CLContextProperty]
unpackContextProperties [] = error "non-exhaustive Context Property list"
unpackContextProperties [x] 
  | x == 0 = []
  | otherwise = error "non-exhaustive Context Property list"
unpackContextProperties (x:y:xs) = y':ys'
   where
      ys' = unpackContextProperties xs 
      ptr = intPtrToPtr . fromIntegral $ y
      y' = case getEnumCL x of
             CL_CONTEXT_PLATFORM_   -> CL_CONTEXT_PLATFORM ptr
             CL_CGL_SHAREGROUP_KHR_ -> CL_CGL_SHAREGROUP_KHR ptr
             CL_GL_CONTEXT_KHR_     -> CL_GL_CONTEXT_KHR ptr
             CL_EGL_DISPLAY_KHR_    -> CL_EGL_DISPLAY_KHR ptr
             CL_GLX_DISPLAY_KHR_    -> CL_GLX_DISPLAY_KHR ptr
             CL_WGL_HDC_KHR_        -> CL_WGL_HDC_KHR ptr

  
mkContextCallback :: (String -> IO ()) -> ContextCallback
mkContextCallback f msg _ _ _ = peekCString msg >>= f

-- | Creates an OpenCL context.
-- An OpenCL context is created with one or more devices. Contexts are used by 
-- the OpenCL runtime for managing objects such as command-queues, memory, 
-- program and kernel objects and for executing kernels on one or more devices 
-- specified in the context.
clCreateContext :: OpenCLLibrary -> [CLContextProperty] -> [CLDeviceID] -> (String -> IO ()) 
                   -> IO CLContext
clCreateContext lib [] devs f = withArray devs $ \pdevs ->
  wrapPError $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    rawClCreateContext lib nullPtr cndevs pdevs fptr nullPtr perr
    where
      cndevs = fromIntegral . length $ devs
clCreateContext lib props devs f = withArray devs $ \pdevs ->
  wrapPError $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    withArray (packContextProperties props) $ \pprops ->
      rawClCreateContext lib pprops cndevs pdevs fptr nullPtr perr    
    where
      cndevs = fromIntegral . length $ devs

-- | Create an OpenCL context from a device type that identifies the specific 
-- device(s) to use.
clCreateContextFromType :: OpenCLLibrary -> [CLContextProperty] -> [CLDeviceType] 
                           -> (String -> IO ()) -> IO CLContext
clCreateContextFromType lib [] xs f = wrapPError $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  rawClCreateContextFromType lib nullPtr types fptr nullPtr perr
    where
      types = bitmaskFromFlags xs
clCreateContextFromType lib props xs f = wrapPError $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  withArray (packContextProperties props) $ \pprops -> 
    rawClCreateContextFromType lib pprops types fptr nullPtr perr
    where
      types = bitmaskFromFlags xs

-- | Increment the context reference count.
-- 'clCreateContext' and 'clCreateContextFromType' perform an implicit retain. 
-- This is very helpful for 3rd party libraries, which typically get a context 
-- passed to them by the application. However, it is possible that the 
-- application may delete the context without informing the library. Allowing 
-- functions to attach to (i.e. retain) and release a context solves the 
-- problem of a context being used by a library no longer being valid.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clRetainContext :: OpenCLLibrary -> CLContext -> IO Bool
clRetainContext lib ctx = wrapCheckSuccess $ rawClRetainContext lib ctx 

-- | Decrement the context reference count.
-- After the context reference count becomes zero and all the objects attached 
-- to context (such as memory objects, command-queues) are released, the 
-- context is deleted.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clReleaseContext :: OpenCLLibrary -> CLContext -> IO Bool
clReleaseContext lib ctx = wrapCheckSuccess $ rawClReleaseContext lib ctx 

getContextInfoSize :: OpenCLLibrary -> CLContext -> CLContextInfo_ -> IO CSize
getContextInfoSize lib ctx infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (rawClGetContextInfo lib ctx infoid 0 nullPtr value_size)
    $ peek value_size

data CLContextInfo = 
     CL_CONTEXT_REFERENCE_COUNT
   | CL_CONTEXT_DEVICES
   | CL_CONTEXT_PROPERTIES
   | CL_CONTEXT_NUM_DEVICES

instance Enum CLContextInfo where
   fromEnum CL_CONTEXT_REFERENCE_COUNT = 0x1080
   fromEnum CL_CONTEXT_DEVICES         = 0x1081
   fromEnum CL_CONTEXT_PROPERTIES      = 0x1082
   fromEnum CL_CONTEXT_NUM_DEVICES     = 0x1083

   toEnum 0x1080 = CL_CONTEXT_REFERENCE_COUNT
   toEnum 0x1081 = CL_CONTEXT_DEVICES
   toEnum 0x1082 = CL_CONTEXT_PROPERTIES
   toEnum 0x1083 = CL_CONTEXT_NUM_DEVICES
   toEnum _ = error "Invalid Context Info value"

-- | Return the context reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in 
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetContextInfo with 'CL_CONTEXT_REFERENCE_COUNT'.
clGetContextReferenceCount :: OpenCLLibrary -> CLContext -> IO CLuint
clGetContextReferenceCount lib ctx =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetContextInfo lib ctx infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_CONTEXT_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the list of devices in context.
--
-- This function execute OpenCL clGetContextInfo with 'CL_CONTEXT_DEVICES'.
clGetContextDevices :: OpenCLLibrary -> CLContext -> IO [CLDeviceID]
clGetContextDevices lib ctx = do
  size <- getContextInfoSize lib ctx infoid
  let n = (fromIntegral size) `div` elemSize 
    
  allocaArray n $ \(buff :: Ptr CLDeviceID) -> do
    whenSuccess (rawClGetContextInfo lib ctx infoid size (castPtr buff) nullPtr)
      $ peekArray n buff
    where
      infoid = getCLValue CL_CONTEXT_DEVICES
      elemSize = sizeOf (nullPtr :: CLDeviceID)

clGetContextProperties :: OpenCLLibrary -> CLContext -> IO [CLContextProperty]
clGetContextProperties lib ctx = do
  size <- getContextInfoSize lib ctx infoid
  let n = (fromIntegral size) `div` elemSize 
    
  if n == 0 
    then return []
    else allocaArray n $ \(buff :: Ptr CLContextProperty_) ->
      whenSuccess (rawClGetContextInfo lib ctx infoid size (castPtr buff) nullPtr)
        $ fmap unpackContextProperties $ peekArray n buff
    where
      infoid = getCLValue CL_CONTEXT_PROPERTIES
      elemSize = sizeOf (nullPtr :: CLDeviceID)
