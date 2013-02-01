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
module ViperVM.Backends.OpenCL.Memory(
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
import ViperVM.Backends.OpenCL.Types( 
  CLMem, CLImageFormat, CLContext, CLSampler, CLuint, CLbool, CLMemFlags_,
  CLAddressingMode_, CLFilterMode_, CLMemObjectType(..), 
  CLAddressingMode(..), CLFilterMode(..), CLMemFlag(..), CLMemObjectType_, 
  wrapPError, wrapCheckSuccess, wrapGetInfo, whenSuccess, getEnumCL, 
  bitmaskFromFlags, bitmaskToMemFlags, getCLValue )

import ViperVM.Backends.OpenCL.Loader

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
{-| Creates a buffer object. Returns a valid non-zero buffer object if the
buffer object is created successfully. Otherwise, it throws the 'CLError': 

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_BUFFER_SIZE' if size is 0 or is greater than
'clDeviceMaxMemAllocSize' value for all devices in context.

 * 'CL_INVALID_HOST_PTR' if host_ptr is NULL and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for buffer object.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateBuffer :: Integral a => OpenCLLibrary -> CLContext -> [CLMemFlag] -> (a, Ptr ()) -> IO CLMem
clCreateBuffer lib ctx xs (sbuff,buff) = wrapPError $ \perr -> do
  rawClCreateBuffer lib ctx flags (fromIntegral sbuff) buff perr
    where
      flags = bitmaskFromFlags xs

{-| Creates an OpenCL buffer object from an OpenGL buffer object. Returns a valid non-zero OpenCL buffer object if the buffer object is created successfully. Otherwise it throws the 'CLError':
 * 'CL_INVALID_CONTEXT' if context is not a valid context or was not created from a GL context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_GL_OBJECT' if bufobj is not a GL buffer object or is a GL buffer object but does not have an existing data store.

 * 'CL_OUT_OF_RESOURCES' if there is a failure to allocate resources required by the OpenCL implementation on the device.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required by the OpenCL implementation on the host.
-}
clCreateFromGLBuffer :: Integral a => OpenCLLibrary -> CLContext -> [CLMemFlag] -> a -> IO CLMem
clCreateFromGLBuffer lib ctx xs glObj = wrapPError $ \perr -> do
  rawClCreateFromGLBuffer lib ctx flags cglObj perr
    where flags = bitmaskFromFlags xs
          cglObj = fromIntegral glObj
    
-- | Increments the memory object reference count. returns 'True' if the
-- function is executed successfully. After the memobj reference count becomes
-- zero and commands queued for execution on a command-queue(s) that use memobj
-- have finished, the memory object is deleted. It returns 'False' if memobj is
-- not a valid memory object.
clRetainMemObject :: OpenCLLibrary -> CLMem -> IO Bool
clRetainMemObject lib mem = wrapCheckSuccess $ rawClRetainMemObject lib mem

-- | Decrements the memory object reference count. After the memobj reference
-- count becomes zero and commands queued for execution on a command-queue(s)
-- that use memobj have finished, the memory object is deleted. Returns 'True'
-- if the function is executed successfully. It returns 'False' if memobj is not
-- a valid memory object.
clReleaseMemObject :: OpenCLLibrary -> CLMem -> IO Bool
clReleaseMemObject lib mem = wrapCheckSuccess $ rawClReleaseMemObject lib mem

-- -----------------------------------------------------------------------------
{-| Creates a 2D image object.

'clCreateImage2D' returns a valid non-zero image object created if the image
object is created successfully. Otherwise, it throws one of the following
'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if values specified in image_format are
not valid.

 * 'CL_INVALID_IMAGE_SIZE' if image_width or image_height are 0 or if they
exceed values specified in 'CL_DEVICE_IMAGE2D_MAX_WIDTH' or
'CL_DEVICE_IMAGE2D_MAX_HEIGHT' respectively for all devices in context or if
values specified by image_row_pitch do not follow rules described in the
argument description above.

 * 'CL_INVALID_HOST_PTR' if host_ptr is 'nullPtr' and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not 'nullPtr' but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_IMAGE_FORMAT_NOT_SUPPORTED' if the image_format is not supported.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for image object.

 * 'CL_INVALID_OPERATION' if there are no devices in context that support images
(i.e. 'CL_DEVICE_IMAGE_SUPPORT' (specified in the table of OpenCL Device Queries
for 'clGetDeviceInfo') is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}

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

{-| Creates a 3D image object.

'clCreateImage3D' returns a valid non-zero image object created if the image
object is created successfully. Otherwise, it throws one of the following
'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if values specified in image_format are
not valid.

 * 'CL_INVALID_IMAGE_SIZE' if image_width, image_height are 0 or if image_depth
less than or equal to 1 or if they exceed values specified in
'CL_DEVICE_IMAGE3D_MAX_WIDTH', CL_DEVICE_IMAGE3D_MAX_HEIGHT' or
'CL_DEVICE_IMAGE3D_MAX_DEPTH' respectively for all devices in context or if
values specified by image_row_pitch and image_slice_pitch do not follow rules
described in the argument description above.

 * 'CL_INVALID_HOST_PTR' if host_ptr is 'nullPtr' and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not 'nullPtr' but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_IMAGE_FORMAT_NOT_SUPPORTED' if the image_format is not supported.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for image object.

 * 'CL_INVALID_OPERATION' if there are no devices in context that support images
(i.e. 'CL_DEVICE_IMAGE_SUPPORT' (specified in the table of OpenCL Device Queries
for clGetDeviceInfo) is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
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

{-| Creates a 2D OpenCL image object from an existing OpenGL texture.

'clCreateFromGLTexture2D' returns a non-zero image object if the image
object is created successfully. Otherwise, it throws one of the
following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context or was not
created from a GL context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid or if
value specified in texture_target is not one of the values specified
in the description of texture_target.

 * 'CL_INVALID_MIPLEVEL' if miplevel is less than the value of
levelbase (for OpenGL implementations) or zero (for OpenGL ES
implementations); or greater than the value of q (for both OpenGL and
OpenGL ES). levelbase and q are defined for the texture in section
3.8.10 (Texture Completeness) of the OpenGL 2.1 specification and
section 3.7.10 of the OpenGL ES 2.0 specification.

 * 'CL_INVALID_MIPLEVEL' if miplevel is greater than zero and the
OpenGL implementation does not support creating from non-zero mipmap
levels.

 * 'CL_INVALID_GL_OBJECT' if texture is not a GL texture object whose
type matches texture_target, if the specified miplevel of texture is
not defined, or if the width or height of the specified miplevel is
zero.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if the OpenGL texture internal
format does not map to a supported OpenCL image format.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources
required by the OpenCL implementation on the host.

-}
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
  
{-| Get the list of image formats supported by an OpenCL
implementation. 'clGetSupportedImageFormats' can be used to get the list of
image formats supported by an OpenCL implementation when the following
information about an image memory object is specified:

 * Context
 * Image type - 2D or 3D image
 * Image object allocation information

Throws 'CL_INVALID_CONTEXT' if context is not a valid context, throws
'CL_INVALID_VALUE' if flags or image_type are not valid.

-}
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

-- -----------------------------------------------------------------------------
#c
enum CLImageInfo {
  cL_IMAGE_FORMAT=CL_IMAGE_FORMAT,
  cL_IMAGE_ELEMENT_SIZE=CL_IMAGE_ELEMENT_SIZE,
  cL_IMAGE_ROW_PITCH=CL_IMAGE_ROW_PITCH,
  cL_IMAGE_SLICE_PITCH=CL_IMAGE_SLICE_PITCH,
  cL_IMAGE_WIDTH=CL_IMAGE_WIDTH,
  cL_IMAGE_HEIGHT=CL_IMAGE_HEIGHT,
  cL_IMAGE_DEPTH=CL_IMAGE_DEPTH,
  };
#endc
{#enum CLImageInfo {upcaseFirstLetter} #}

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

-- -----------------------------------------------------------------------------
#c
enum CLMemInfo {
  cL_MEM_TYPE=CL_MEM_TYPE,
  cL_MEM_FLAGS=CL_MEM_FLAGS,
  cL_MEM_SIZE=CL_MEM_SIZE,
  cL_MEM_HOST_PTR=CL_MEM_HOST_PTR,
  cL_MEM_MAP_COUNT=CL_MEM_MAP_COUNT,
  cL_MEM_REFERENCE_COUNT=CL_MEM_REFERENCE_COUNT,
  cL_MEM_CONTEXT=CL_MEM_CONTEXT,
  };
#endc
{#enum CLMemInfo {upcaseFirstLetter} #}

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

-- -----------------------------------------------------------------------------
{-| Creates a sampler object. A sampler object describes how to sample an image
when the image is read in the kernel. The built-in functions to read from an
image in a kernel take a sampler as an argument. The sampler arguments to the
image read function can be sampler objects created using OpenCL functions and
passed as argument values to the kernel or can be samplers declared inside a
kernel. In this section we discuss how sampler objects are created using OpenCL
functions.

Returns a valid non-zero sampler object if the sampler object is created
successfully. Otherwise, it throws one of the following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if addressing_mode, filter_mode, or normalized_coords or a
combination of these argument values are not valid.

 * 'CL_INVALID_OPERATION' if images are not supported by any device associated
with context (i.e. 'CL_DEVICE_IMAGE_SUPPORT' specified in the table of OpenCL
Device Queries for clGetDeviceInfo is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
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

#c
enum CLSamplerInfo {
  cL_SAMPLER_REFERENCE_COUNT=CL_SAMPLER_REFERENCE_COUNT,
  cL_SAMPLER_CONTEXT=CL_SAMPLER_CONTEXT,
  cL_SAMPLER_ADDRESSING_MODE=CL_SAMPLER_ADDRESSING_MODE,
  cL_SAMPLER_FILTER_MODE=CL_SAMPLER_FILTER_MODE,
  cL_SAMPLER_NORMALIZED_COORDS=CL_SAMPLER_NORMALIZED_COORDS
  };
#endc
{#enum CLSamplerInfo {upcaseFirstLetter} #}

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

-- -----------------------------------------------------------------------------
