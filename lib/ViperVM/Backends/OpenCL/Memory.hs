{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Backends.OpenCL.Memory(
   Memory(..), 
   memoryGlobalSize, memoryName,
  -- * Memory Functions
  clCreateBuffer, clRetainMemObject, clReleaseMemObject, clGetMemType, 
  clGetMemFlags, clGetMemSize, clGetMemHostPtr, clGetMemMapCount, 
  clGetMemReferenceCount, clGetMemContext, clCreateFromGLBuffer,
  -- * Image Functions
  clCreateImage2D, clCreateImage3D, clCreateFromGLTexture2D,
  clGetSupportedImageFormats, clGetImageFormat, clGetImageElementSize, 
  clGetImageRowPitch, clGetImageSlicePitch, clGetImageWidth, clGetImageHeight, 
  clGetImageDepth,
  -- * Sampler Functions
  clCreateSampler, clRetainSampler, clReleaseSampler, clGetSamplerReferenceCount, 
  clGetSamplerContext, clGetSamplerAddressingMode, clGetSamplerFilterMode, 
  clGetSamplerNormalizedCoords
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Query


data Memory = Memory {
   memoryLibrary :: OpenCLLibrary,
   memoryContext :: CLContext,
   memoryDevice  :: CLDeviceID
}

instance Eq Memory where
   (==) a b = memoryDevice a == memoryDevice b

instance Ord Memory where
   compare a b = compare (memoryDevice a) (memoryDevice b)

instance Show Memory where
   show m = "(" ++ show (memoryDevice m) ++ ")"


memoryGlobalSize :: Memory -> IO Word64
memoryGlobalSize m = clGetDeviceGlobalMemSize (memoryLibrary m) (memoryDevice m)

memoryName :: Memory -> IO String
memoryName m = clGetDeviceName (memoryLibrary m) (memoryDevice m)






clCreateBuffer :: Integral a => OpenCLLibrary -> CLContext -> [CLMemFlag] -> (a, Ptr ()) -> IO CLMem
clCreateBuffer lib ctx xs (sbuff,buff) = wrapPError $ \perr -> do
  rawClCreateBuffer lib ctx flags (fromIntegral sbuff) buff perr
    where
      flags = bitmaskFromFlags xs

clCreateFromGLBuffer :: Integral a => OpenCLLibrary -> CLContext -> [CLMemFlag] -> a -> IO CLMem
clCreateFromGLBuffer lib ctx xs glObj = wrapPError $ \perr -> do
  rawClCreateFromGLBuffer lib ctx flags cglObj perr
    where flags = bitmaskFromFlags xs
          cglObj = fromIntegral glObj
    
clRetainMemObject :: OpenCLLibrary -> CLMem -> IO Bool
clRetainMemObject lib mem = wrapCheckSuccess $ rawClRetainMemObject lib mem

clReleaseMemObject :: OpenCLLibrary -> CLMem -> IO Bool
clReleaseMemObject lib mem = wrapCheckSuccess $ rawClReleaseMemObject lib mem

clCreateImage2D :: Integral a => OpenCLLibrary -> CLContext -- ^ A valid OpenCL context on which
                                           -- the image object is to be created.
                   -> [CLMemFlag] -- ^ A list of flags that is used to specify
                                  -- allocation and usage information about the
                                  -- image memory object being created.
                   -> CLImageFormat -- ^ Structure that describes format
                                    -- properties of the image to be allocated.
                   -> a -- ^ The width of the image in pixels. It must be values
                        -- greater than or equal to 1.
                   -> a -- ^ The height of the image in pixels. It must be
                        -- values greater than or equal to 1.
                   -> a -- ^ The scan-line pitch in bytes. This must be 0 if
                        -- host_ptr is 'nullPtr' and can be either 0 or greater
                        -- than or equal to image_width * size of element in
                        -- bytes if host_ptr is not 'nullPtr'. If host_ptr is
                        -- not 'nullPtr' and image_row_pitch is equal to 0,
                        -- image_row_pitch is calculated as image_width * size
                        -- of element in bytes. If image_row_pitch is not 0, it
                        -- must be a multiple of the image element size in
                        -- bytes.
                   -> Ptr () -- ^ A pointer to the image data that may already
                             -- be allocated by the application. The size of the
                             -- buffer that host_ptr points to must be greater
                             -- than or equal to image_row_pitch *
                             -- image_height. The size of each element in bytes
                             -- must be a power of 2. The image data specified
                             -- by host_ptr is stored as a linear sequence of
                             -- adjacent scanlines. Each scanline is stored as a
                             -- linear sequence of image elements.
                   -> IO CLMem
clCreateImage2D lib ctx xs fmt iw ih irp ptr = wrapPError $ \perr -> with fmt $ \pfmt -> do
  rawClCreateImage2D lib ctx flags pfmt ciw cih cirp ptr perr
    where
      flags = bitmaskFromFlags xs
      ciw = fromIntegral iw
      cih = fromIntegral ih
      cirp = fromIntegral irp

clCreateImage3D :: Integral a => OpenCLLibrary
	           -> CLContext -- ^ A valid OpenCL context on which
                                           -- the image object is to be created.
                   -> [CLMemFlag] -- ^ A list of flags that is used to specify
                                  -- allocation and usage information about the
                                  -- image memory object being created.
                   -> CLImageFormat -- ^ Structure that describes format
                                    -- properties of the image to be allocated.
                   -> a -- ^ The width of the image in pixels. It must be values
                        -- greater than or equal to 1.
                   -> a -- ^ The height of the image in pixels. It must be
                        -- values greater than or equal to 1.
                   -> a -- ^ The depth of the image in pixels. This must be a
                        -- value greater than 1.
                   -> a -- ^ The scan-line pitch in bytes. This must be 0 if
                        -- host_ptr is 'nullPtr' and can be either 0 or greater
                        -- than or equal to image_width * size of element in
                        -- bytes if host_ptr is not 'nullPtr'. If host_ptr is
                        -- not 'nullPtr' and image_row_pitch is equal to 0,
                        -- image_row_pitch is calculated as image_width * size
                        -- of element in bytes. If image_row_pitch is not 0, it
                        -- must be a multiple of the image element size in
                        -- bytes.
                   -> a -- ^ The size in bytes of each 2D slice in the 3D
                        -- image. This must be 0 if host_ptr is 'nullPtr' and
                        -- can be either 0 or greater than or equal to
                        -- image_row_pitch * image_height if host_ptr is not
                        -- 'nullPtr'. If host_ptr is not 'nullPtr' and
                        -- image_slice_pitch equal to 0, image_slice_pitch is
                        -- calculated as image_row_pitch * image_height. If
                        -- image_slice_pitch is not 0, it must be a multiple of
                        -- the image_row_pitch.
                   -> Ptr () -- ^ A pointer to the image data that may already
                             -- be allocated by the application. The size of the
                             -- buffer that host_ptr points to must be greater
                             -- than or equal to image_slice_pitch *
                             -- image_depth. The size of each element in bytes
                             -- must be a power of 2. The image data specified
                             -- by host_ptr is stored as a linear sequence of
                             -- adjacent 2D slices. Each 2D slice is a linear
                             -- sequence of adjacent scanlines. Each scanline is
                             -- a linear sequence of image elements.
                   -> IO CLMem
clCreateImage3D lib ctx xs fmt iw ih idepth irp isp ptr = wrapPError $ \perr -> with fmt $ \pfmt -> do
  rawClCreateImage3D lib ctx flags pfmt ciw cih cid cirp cisp ptr perr
    where
      flags = bitmaskFromFlags xs
      ciw = fromIntegral iw
      cih = fromIntegral ih
      cid = fromIntegral idepth
      cirp = fromIntegral irp
      cisp = fromIntegral isp  

clCreateFromGLTexture2D :: (Integral a, Integral b, Integral c) =>
			   OpenCLLibrary -> 
                           CLContext -- ^ A valid OpenCL context in
                                     -- which the image object is to
                                     -- be created.
                        -> [CLMemFlag] -- ^ A list of flags that is
                                       -- used to specify usage
                                       -- information about the image
                                       -- memory object being created.
                        -> a -- ^ The OpenGL image type of the texture
                             -- (e.g. GL_TEXTURE_2D)
                        -> b -- ^ The mipmap level to be used.
                        -> c -- ^ The GL texture object name.
                        -> IO CLMem
clCreateFromGLTexture2D lib ctx xs texType mipLevel tex = 
  wrapPError $ rawClCreateFromGLTexture2D lib ctx flags cTexType cMip cTex
    where flags = bitmaskFromFlags xs
          cTexType = fromIntegral texType
          cMip = fromIntegral mipLevel
          cTex = fromIntegral tex
      
getNumSupportedImageFormats :: OpenCLLibrary -> CLContext -> [CLMemFlag] -> CLMemObjectType -> IO CLuint
getNumSupportedImageFormats lib ctx xs mtype = alloca $ \(value_size :: Ptr CLuint) -> do
  whenSuccess (rawClGetSupportedImageFormats lib ctx flags (getCLValue mtype) 0 nullPtr value_size)
    $ peek value_size
    where
      flags = bitmaskFromFlags xs
  
clGetSupportedImageFormats :: OpenCLLibrary 
			      -> CLContext -- ^ A valid OpenCL context on which the
                                        -- image object(s) will be created.
                              -> [CLMemFlag] -- ^ A bit-field that is used to
                                             -- specify allocation and usage
                                             -- information about the image
                                             -- memory object.
                              -> CLMemObjectType -- ^ Describes the image type
                                                 -- and must be either
                                                 -- 'CL_MEM_OBJECT_IMAGE2D' or
                                                 -- 'CL_MEM_OBJECT_IMAGE3D'.
                              -> IO [CLImageFormat]
clGetSupportedImageFormats lib ctx xs mtype = do
  num <- getNumSupportedImageFormats lib ctx xs mtype
  allocaArray (fromIntegral num) $ \(buff :: Ptr CLImageFormat) -> do
    whenSuccess (rawClGetSupportedImageFormats lib ctx flags (getCLValue mtype) num (castPtr buff) nullPtr)
      $ peekArray (fromIntegral num) buff
    where
      flags = bitmaskFromFlags xs


data CLImageInfo = 
     CL_IMAGE_FORMAT
   | CL_IMAGE_ELEMENT_SIZE
   | CL_IMAGE_ROW_PITCH
   | CL_IMAGE_SLICE_PITCH
   | CL_IMAGE_WIDTH
   | CL_IMAGE_HEIGHT
   | CL_IMAGE_DEPTH
   | CL_IMAGE_ARRAY_SIZE
   | CL_IMAGE_BUFFER
   | CL_IMAGE_NUM_MIP_LEVELS
   | CL_IMAGE_NUM_SAMPLES

instance Enum CLImageInfo where
   fromEnum CL_IMAGE_FORMAT          = 0x1110
   fromEnum CL_IMAGE_ELEMENT_SIZE    = 0x1111
   fromEnum CL_IMAGE_ROW_PITCH       = 0x1112
   fromEnum CL_IMAGE_SLICE_PITCH     = 0x1113
   fromEnum CL_IMAGE_WIDTH           = 0x1114
   fromEnum CL_IMAGE_HEIGHT          = 0x1115
   fromEnum CL_IMAGE_DEPTH           = 0x1116
   fromEnum CL_IMAGE_ARRAY_SIZE      = 0x1117
   fromEnum CL_IMAGE_BUFFER          = 0x1118
   fromEnum CL_IMAGE_NUM_MIP_LEVELS  = 0x1119
   fromEnum CL_IMAGE_NUM_SAMPLES     = 0x111A

   toEnum 0x1110 = CL_IMAGE_FORMAT
   toEnum 0x1111 = CL_IMAGE_ELEMENT_SIZE
   toEnum 0x1112 = CL_IMAGE_ROW_PITCH
   toEnum 0x1113 = CL_IMAGE_SLICE_PITCH
   toEnum 0x1114 = CL_IMAGE_WIDTH
   toEnum 0x1115 = CL_IMAGE_HEIGHT
   toEnum 0x1116 = CL_IMAGE_DEPTH
   toEnum 0x1117 = CL_IMAGE_ARRAY_SIZE
   toEnum 0x1118 = CL_IMAGE_BUFFER
   toEnum 0x1119 = CL_IMAGE_NUM_MIP_LEVELS
   toEnum 0x111A = CL_IMAGE_NUM_SAMPLES
   toEnum _ = error "Invalid Image Info value"
   

-- | Return image format descriptor specified when image is created with
-- clCreateImage2D or clCreateImage3D.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_FORMAT'.
clGetImageFormat :: OpenCLLibrary -> CLMem -> IO CLImageFormat
clGetImageFormat lib mem =
  wrapGetInfo (\(dat :: Ptr CLImageFormat) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_FORMAT
      size = fromIntegral $ sizeOf (undefined :: CLImageFormat)
  
-- | Return size of each element of the image memory object given by image. An
-- element is made up of n channels. The value of n is given in 'CLImageFormat'
-- descriptor.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_ELEMENT_SIZE'.
clGetImageElementSize :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageElementSize lib mem =
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_ELEMENT_SIZE
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return size in bytes of a row of elements of the image object given by
-- image.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_ROW_PITCH'.
clGetImageRowPitch :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageRowPitch lib mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_ROW_PITCH
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return size in bytes of a 2D slice for the 3D image object given by
-- image. For a 2D image object this value will be 0.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_SLICE_PITCH'.
clGetImageSlicePitch :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageSlicePitch lib mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_SLICE_PITCH
      size = fromIntegral $ sizeOf (undefined :: CSize)      
      
-- | Return width of image in pixels.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_WIDTH'.
clGetImageWidth :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageWidth lib mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_WIDTH
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return height of image in pixels.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_HEIGHT'.
clGetImageHeight :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageHeight lib mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_HEIGHT
      size = fromIntegral $ sizeOf (undefined :: CSize)

-- | Return depth of the image in pixels. For a 2D image, depth equals 0.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_DEPTH'.
clGetImageDepth :: OpenCLLibrary -> CLMem -> IO CSize      
clGetImageDepth lib mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                rawClGetImageInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_DEPTH
      size = fromIntegral $ sizeOf (undefined :: CSize)


data CLMemInfo =
     CL_MEM_TYPE
   | CL_MEM_FLAGS
   | CL_MEM_SIZE
   | CL_MEM_HOST_PTR
   | CL_MEM_MAP_COUNT
   | CL_MEM_REFERENCE_COUNT
   | CL_MEM_CONTEXT
   | CL_MEM_ASSOCIATED_MEMOBJECT
   | CL_MEM_OFFSET

instance Enum CLMemInfo where
   fromEnum CL_MEM_TYPE                  = 0x1100
   fromEnum CL_MEM_FLAGS                 = 0x1101
   fromEnum CL_MEM_SIZE                  = 0x1102
   fromEnum CL_MEM_HOST_PTR              = 0x1103
   fromEnum CL_MEM_MAP_COUNT             = 0x1104
   fromEnum CL_MEM_REFERENCE_COUNT       = 0x1105
   fromEnum CL_MEM_CONTEXT               = 0x1106
   fromEnum CL_MEM_ASSOCIATED_MEMOBJECT  = 0x1107
   fromEnum CL_MEM_OFFSET                = 0x1108

   toEnum 0x1100 = CL_MEM_TYPE
   toEnum 0x1101 = CL_MEM_FLAGS
   toEnum 0x1102 = CL_MEM_SIZE
   toEnum 0x1103 = CL_MEM_HOST_PTR
   toEnum 0x1104 = CL_MEM_MAP_COUNT
   toEnum 0x1105 = CL_MEM_REFERENCE_COUNT
   toEnum 0x1106 = CL_MEM_CONTEXT
   toEnum 0x1107 = CL_MEM_ASSOCIATED_MEMOBJECT
   toEnum 0x1108 = CL_MEM_OFFSET
   toEnum _ = error "Invalid Mem Info value"

-- | Returns the mem object type.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_TYPE'.
clGetMemType :: OpenCLLibrary -> CLMem -> IO CLMemObjectType
clGetMemType lib mem =
    wrapGetInfo (\(dat :: Ptr CLMemObjectType_) ->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_MEM_TYPE
      size = fromIntegral $ sizeOf (0::CLMemObjectType_)

-- | Return the flags argument value specified when memobj was created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_FLAGS'.
clGetMemFlags :: OpenCLLibrary -> CLMem -> IO [CLMemFlag]
clGetMemFlags lib mem =
    wrapGetInfo (\(dat :: Ptr CLMemFlags_)->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) bitmaskToMemFlags
    where 
      infoid = getCLValue CL_MEM_FLAGS
      size = fromIntegral $ sizeOf (0::CLMemFlags_)

-- | Return actual size of memobj in bytes.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_SIZE'.
clGetMemSize :: OpenCLLibrary -> CLMem -> IO CSize
clGetMemSize lib mem =
    wrapGetInfo (\(dat :: Ptr CSize)->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_SIZE
      size = fromIntegral $ sizeOf (0::CSize)

-- | Return the host_ptr argument value specified when memobj is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_HOST_PTR'.
clGetMemHostPtr :: OpenCLLibrary -> CLMem -> IO (Ptr ())
clGetMemHostPtr lib mem =
    wrapGetInfo (\(dat :: Ptr (Ptr ()))->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_HOST_PTR
      size = fromIntegral $ sizeOf (nullPtr::Ptr ())

-- | Map count. The map count returned should be considered immediately
-- stale. It is unsuitable for general use in applications. This feature is
-- provided for debugging.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_MAP_COUNT'.
clGetMemMapCount :: OpenCLLibrary -> CLMem -> IO CLuint
clGetMemMapCount lib mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_MAP_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return memobj reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_REFERENCE_COUNT'.
clGetMemReferenceCount :: OpenCLLibrary -> CLMem -> IO CLuint
clGetMemReferenceCount lib mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return context specified when memory object is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_CONTEXT'.
clGetMemContext :: OpenCLLibrary -> CLMem -> IO CLContext
clGetMemContext lib mem =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        rawClGetMemObjectInfo lib mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_CONTEXT
      size = fromIntegral $ sizeOf (0 :: CLuint)


clCreateSampler :: OpenCLLibrary -> CLContext -> Bool -> CLAddressingMode -> CLFilterMode 
                   -> IO CLSampler
clCreateSampler lib ctx norm am fm = wrapPError $ \perr -> do
  rawClCreateSampler lib ctx (fromBool norm) (getCLValue am) (getCLValue fm) perr

-- | Increments the sampler reference count. 'clCreateSampler' does an implicit
-- retain. Returns 'True' if the function is executed successfully. It returns
-- 'False' if sampler is not a valid sampler object.
clRetainSampler :: OpenCLLibrary -> CLSampler -> IO Bool
clRetainSampler lib mem = wrapCheckSuccess $ rawClRetainSampler lib mem

-- | Decrements the sampler reference count. The sampler object is deleted after
-- the reference count becomes zero and commands queued for execution on a
-- command-queue(s) that use sampler have finished. 'clReleaseSampler' returns
-- 'True' if the function is executed successfully. It returns 'False' if
-- sampler is not a valid sampler object.
clReleaseSampler :: OpenCLLibrary -> CLSampler -> IO Bool
clReleaseSampler lib mem = wrapCheckSuccess $ rawClReleaseSampler lib mem

data CLSamplerInfo = 
     CL_SAMPLER_REFERENCE_COUNT
   | CL_SAMPLER_CONTEXT
   | CL_SAMPLER_NORMALIZED_COORDS
   | CL_SAMPLER_ADDRESSING_MODE
   | CL_SAMPLER_FILTER_MODE

instance Enum CLSamplerInfo where
   fromEnum CL_SAMPLER_REFERENCE_COUNT   = 0x1150
   fromEnum CL_SAMPLER_CONTEXT           = 0x1151
   fromEnum CL_SAMPLER_NORMALIZED_COORDS = 0x1152
   fromEnum CL_SAMPLER_ADDRESSING_MODE   = 0x1153
   fromEnum CL_SAMPLER_FILTER_MODE       = 0x1154

   toEnum 0x1150 = CL_SAMPLER_REFERENCE_COUNT
   toEnum 0x1151 = CL_SAMPLER_CONTEXT
   toEnum 0x1152 = CL_SAMPLER_NORMALIZED_COORDS
   toEnum 0x1153 = CL_SAMPLER_ADDRESSING_MODE
   toEnum 0x1154 = CL_SAMPLER_FILTER_MODE
   toEnum _ = error "Invalid Sampler Info value"

-- | Return the sampler reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_REFERENCE_COUNT'.
clGetSamplerReferenceCount :: OpenCLLibrary -> CLSampler -> IO CLuint
clGetSamplerReferenceCount lib sam =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        rawClGetSamplerInfo lib sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return the context specified when the sampler is created.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_CONTEXT'.
clGetSamplerContext :: OpenCLLibrary -> CLSampler -> IO CLContext
clGetSamplerContext lib sam =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        rawClGetSamplerInfo lib sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr :: CLContext)

-- | Return the value specified by addressing_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_ADDRESSING_MODE'.
clGetSamplerAddressingMode :: OpenCLLibrary -> CLSampler -> IO CLAddressingMode
clGetSamplerAddressingMode lib sam =
    wrapGetInfo (\(dat :: Ptr CLAddressingMode_)->
        rawClGetSamplerInfo lib sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_ADDRESSING_MODE
      size = fromIntegral $ sizeOf (0 :: CLAddressingMode_)

-- | Return the value specified by filter_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_FILTER_MODE'.
clGetSamplerFilterMode :: OpenCLLibrary -> CLSampler -> IO CLFilterMode
clGetSamplerFilterMode lib sam =
    wrapGetInfo (\(dat :: Ptr CLFilterMode_)->
        rawClGetSamplerInfo lib sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_FILTER_MODE
      size = fromIntegral $ sizeOf (0 :: CLFilterMode_)

-- | Return the value specified by normalized_coords argument to
-- clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_NORMALIZED_COORDS'.
clGetSamplerNormalizedCoords :: OpenCLLibrary -> CLSampler -> IO Bool
clGetSamplerNormalizedCoords lib sam =
    wrapGetInfo (\(dat :: Ptr CLbool)->
        rawClGetSamplerInfo lib sam infoid size (castPtr dat)) (/=0)
    where 
      infoid = getCLValue CL_SAMPLER_NORMALIZED_COORDS
      size = fromIntegral $ sizeOf (0 :: CLbool)

