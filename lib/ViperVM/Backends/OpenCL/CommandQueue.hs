{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Backends.OpenCL.CommandQueue(
  -- * Types
  CLCommandQueue, CLCommandQueueProperty(..), CLMapFlag(..),
  -- * Command Queue Functions
  clCreateCommandQueue, clRetainCommandQueue, clReleaseCommandQueue,
  clGetCommandQueueContext, clGetCommandQueueDevice, 
  clGetCommandQueueReferenceCount, clGetCommandQueueProperties,
  clSetCommandQueueProperty,
  -- * Memory Commands
  clEnqueueReadBuffer, clEnqueueWriteBuffer, clEnqueueReadImage, 
  clEnqueueWriteImage, clEnqueueCopyImage, clEnqueueCopyImageToBuffer,
  clEnqueueCopyBufferToImage, clEnqueueMapBuffer, clEnqueueMapImage,
  clEnqueueUnmapMemObject,
  -- * Executing Kernels
  clEnqueueNDRangeKernel, clEnqueueTask, clEnqueueNativeKernel, 
  clEnqueueMarker, clEnqueueWaitForEvents, clEnqueueBarrier,
  -- * Flush and Finish
  clFlush, clFinish
  ) where

import Foreign
import Foreign.C.Types
import ViperVM.Backends.OpenCL.Types(
  CLCommandQueueInfo(..),
  CLint, CLuint, CLCommandQueueProperty_,  
  CLMapFlag(..), CLCommandQueue, CLDeviceID, CLContext, 
  CLCommandQueueProperty(..), CLEvent, CLMem, CLKernel,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo, getCLValue, 
  bitmaskToCommandQueueProperties, bitmaskFromFlags,
  withMaybeArray, wrapNativeKernelCallback)

import ViperVM.Backends.OpenCL.Loader

clCreateCommandQueue :: OpenCLLibrary -> CLContext -> CLDeviceID -> [CLCommandQueueProperty] 
                     -> IO CLCommandQueue
clCreateCommandQueue lib ctx did xs = wrapPError $ \perr -> do
  rawClCreateCommandQueue lib ctx did props perr
    where
      props = bitmaskFromFlags xs

clRetainCommandQueue :: OpenCLLibrary -> CLCommandQueue -> IO Bool
clRetainCommandQueue lib = wrapCheckSuccess . (rawClRetainCommandQueue lib)

clReleaseCommandQueue :: OpenCLLibrary -> CLCommandQueue -> IO Bool
clReleaseCommandQueue lib = wrapCheckSuccess . (rawClReleaseCommandQueue lib)

-- | Return the context specified when the command-queue is created.
--
-- This function execute OpenCL clGetCommandQueueInfo with 'CL_QUEUE_CONTEXT'.
clGetCommandQueueContext :: OpenCLLibrary -> CLCommandQueue -> IO CLContext
clGetCommandQueueContext lib cq =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        rawClGetCommandQueueInfo lib cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the device specified when the command-queue is created.
--
-- This function execute OpenCL clGetCommandQueueInfo with 'CL_QUEUE_DEVICE'.
clGetCommandQueueDevice :: OpenCLLibrary -> CLCommandQueue -> IO CLDeviceID
clGetCommandQueueDevice lib cq =
    wrapGetInfo (\(dat :: Ptr CLDeviceID) ->
        rawClGetCommandQueueInfo lib cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_DEVICE
      size = fromIntegral $ sizeOf (nullPtr::CLDeviceID)

-- | Return the command-queue reference count.
-- The reference count returned should be considered immediately stale. It is 
-- unsuitable for general use in applications. This feature is provided for 
-- identifying memory leaks.
--
-- This function execute OpenCL clGetCommandQueueInfo with
-- 'CL_QUEUE_REFERENCE_COUNT'.
clGetCommandQueueReferenceCount :: OpenCLLibrary -> CLCommandQueue -> IO CLuint
clGetCommandQueueReferenceCount lib cq =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        rawClGetCommandQueueInfo lib cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)


-- | Return the currently specified properties for the command-queue. These 
-- properties are specified by the properties argument in 'clCreateCommandQueue'
-- , and can be changed by 'clSetCommandQueueProperty'.
--
-- This function execute OpenCL clGetCommandQueueInfo with
-- 'CL_QUEUE_PROPERTIES'.
clGetCommandQueueProperties :: OpenCLLibrary -> CLCommandQueue -> IO [CLCommandQueueProperty]
clGetCommandQueueProperties lib cq =
    wrapGetInfo (\(dat :: Ptr CLCommandQueueProperty_) ->
        rawClGetCommandQueueInfo lib cq infoid size (castPtr dat)) bitmaskToCommandQueueProperties
    where 
      infoid = getCLValue CL_QUEUE_PROPERTIES
      size = fromIntegral $ sizeOf (0::CLCommandQueueProperty_)

clSetCommandQueueProperty :: OpenCLLibrary -> CLCommandQueue -> [CLCommandQueueProperty] -> Bool 
                          -> IO [CLCommandQueueProperty]
clSetCommandQueueProperty lib cq xs val = alloca 
                                      $ \(dat :: Ptr CLCommandQueueProperty_) 
                                        -> whenSuccess (f dat)
                                           $ fmap bitmaskToCommandQueueProperties $ peek dat
    where
      f = rawClSetCommandQueueProperty lib cq props (fromBool val)
      props = bitmaskFromFlags xs

clEnqueue :: (CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint) -> [CLEvent] 
             -> IO CLEvent
clEnqueue f [] = alloca $ \event -> whenSuccess (f 0 nullPtr event)
                                    $ peek event
clEnqueue f events = allocaArray nevents $ \pevents -> do
  pokeArray pevents events
  alloca $ \event -> whenSuccess (f cnevents pevents event)
                     $ peek event
    where
      nevents = length events
      cnevents = fromIntegral nevents

clEnqueueReadBuffer :: Integral a => OpenCLLibrary -> CLCommandQueue -> CLMem -> Bool -> a -> a
                       -> Ptr () -> [CLEvent] -> IO CLEvent
clEnqueueReadBuffer lib cq mem check off size dat = clEnqueue (rawClEnqueueReadBuffer lib cq mem (fromBool check) (fromIntegral off) (fromIntegral size) dat)

clEnqueueWriteBuffer :: Integral a => OpenCLLibrary -> CLCommandQueue -> CLMem -> Bool -> a -> a
                       -> Ptr () -> [CLEvent] -> IO CLEvent
clEnqueueWriteBuffer lib cq mem check off size dat = clEnqueue (rawClEnqueueWriteBuffer lib cq mem (fromBool check) (fromIntegral off) (fromIntegral size) dat)

clEnqueueReadImage :: Integral a 
                      => OpenCLLibrary
                      -> CLCommandQueue -- ^ Refers to the command-queue in
                                        -- which the read command will be
                                        -- queued. command_queue and image must
                                        -- be created with the same OpenCL
                                        -- contex
                      -> CLMem -- ^ Refers to a valid 2D or 3D image object.
                      -> Bool -- ^ Indicates if the read operations are blocking
                              -- or non-blocking.
                      -> (a,a,a) -- ^ Defines the (x, y, z) offset in pixels in
                                 -- the image from where to read. If image is a
                                 -- 2D image object, the z value given must be
                                 -- 0.
                      -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                 -- pixels of the 2D or 3D rectangle being
                                 -- read. If image is a 2D image object, the
                                 -- depth value given must be 1.
                      -> a -- ^ The length of each row in bytes. This value must
                           -- be greater than or equal to the element size in
                           -- bytes * width. If row_pitch is set to 0, the
                           -- appropriate row pitch is calculated based on the
                           -- size of each element in bytes multiplied by width.
                      -> a -- ^ Size in bytes of the 2D slice of the 3D region
                           -- of a 3D image being read. This must be 0 if image
                           -- is a 2D image. This value must be greater than or
                           -- equal to row_pitch * height. If slice_pitch is set
                           -- to 0, the appropriate slice pitch is calculated
                           -- based on the row_pitch * height.
                      -> Ptr () -- ^ The pointer to a buffer in host memory
                                -- where image data is to be read from.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty,
                                   -- then this particular command does not wait
                                   -- on any event to complete. The events
                                   -- specified in the list act as
                                   -- synchronization points. The context
                                   -- associated with events in event_wait_list
                                   -- and command_queue must be the same.
                      -> IO CLEvent
clEnqueueReadImage lib cq mem check (orix,oriy,oriz) (regx,regy,regz) rp sp dat xs = 
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (rawClEnqueueReadImage lib cq mem (fromBool check) pori preg (fromIntegral rp) (fromIntegral sp) dat) xs
                       
clEnqueueWriteImage :: Integral a 
                       => OpenCLLibrary 
                       -> CLCommandQueue -- ^ Refers to the command-queue in
                                         -- which the write command will be
                                         -- queued. command_queue and image must
                                         -- be created with the same OpenCL
                                         -- contex
                       -> CLMem -- ^ Refers to a valid 2D or 3D image object.
                       -> Bool -- ^ Indicates if the write operation is blocking
                               -- or non-blocking.
                       -> (a,a,a) -- ^ Defines the (x, y, z) offset in pixels in
                                  -- the image from where to write or write. If
                                  -- image is a 2D image object, the z value
                                  -- given must be 0.
                       -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                  -- pixels of the 2D or 3D rectangle being
                                  -- write or written. If image is a 2D image
                                  -- object, the depth value given must be 1.
                       -> a -- ^ The length of each row in bytes. This value
                            -- must be greater than or equal to the element size
                            -- in bytes * width. If input_row_pitch is set to 0,
                            -- the appropriate row pitch is calculated based on
                            -- the size of each element in bytes multiplied by
                            -- width.
                       -> a -- ^ Size in bytes of the 2D slice of the 3D region
                            -- of a 3D image being written. This must be 0 if
                            -- image is a 2D image. This value must be greater
                            -- than or equal to row_pitch * height. If
                            -- input_slice_pitch is set to 0, the appropriate
                            -- slice pitch is calculated based on the row_pitch
                            -- * height.
                       -> Ptr () -- ^ The pointer to a buffer in host memory
                                 -- where image data is to be written to.
                       -> [CLEvent] -- ^ Specify events that need to complete
                                    -- before this particular command can be
                                    -- executed. If event_wait_list is empty,
                                    -- then this particular command does not
                                    -- wait on any event to complete. The events
                                    -- specified in event_wait_list act as
                                    -- synchronization points. The context
                                    -- associated with events in event_wait_list
                                    -- and command_queue must be the same.
                       -> IO CLEvent
clEnqueueWriteImage lib cq mem check (orix,oriy,oriz) (regx,regy,regz) rp sp dat xs = 
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (rawClEnqueueWriteImage lib cq mem (fromBool check) pori preg (fromIntegral rp) (fromIntegral sp) dat) xs
                       
clEnqueueCopyImage :: Integral a 
                      => OpenCLLibrary 
                      -> CLCommandQueue -- ^ Refers to the command-queue in
                                        -- which the copy command will be
                                        -- queued. The OpenCL context associated
                                        -- with command_queue, src_image and
                                        -- dst_image must be the same.
                      -> CLMem -- ^ src
                      -> CLMem -- ^ dst
                      -> (a,a,a) -- ^ Defines the starting (x, y, z) location in
                                 -- pixels in src_image from where to start the
                                 -- data copy. If src_image is a 2D image
                                 -- object, the z value given must be 0.
                      -> (a,a,a) -- ^ Defines the starting (x, y, z) location in
                                 -- pixels in dst_image from where to start the
                                 -- data copy. If dst_image is a 2D image
                                 -- object, the z value given must be 0.
                      -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                 -- pixels of the 2D or 3D rectangle to copy. If
                                 -- src_image or dst_image is a 2D image object,
                                 -- the depth value given must be 1.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty, then
                                   -- this particular command does not wait on
                                   -- any event to complete. 
                      -> IO CLEvent
clEnqueueCopyImage lib cq src dst (src_orix,src_oriy,src_oriz) (dst_orix,dst_oriy,dst_oriz) (regx,regy,regz) xs =
  withArray (fmap fromIntegral [src_orix,src_oriy,src_oriz]) $ \psrc_ori -> 
  withArray (fmap fromIntegral [dst_orix,dst_oriy,dst_oriz]) $ \pdst_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (rawClEnqueueCopyImage lib cq src dst psrc_ori pdst_ori preg) xs


clEnqueueCopyImageToBuffer :: Integral a 
                              => OpenCLLibrary 
                              -> CLCommandQueue -- ^ The OpenCL context
                                                -- associated with
                                                -- command_queue, src_image, and
                                                -- dst_buffer must be the same.
                              -> CLMem -- ^ src. A valid image object.
                              -> CLMem -- ^ dst. A valid buffer object.
                              -> (a,a,a) -- ^ Defines the (x, y, z) offset in
                                         -- pixels in the image from where to
                                         -- copy. If src_image is a 2D image
                                         -- object, the z value given must be 0.
                              -> (a,a,a) -- ^ Defines the (width, height, depth)
                                         -- in pixels of the 2D or 3D rectangle
                                         -- to copy. If src_image is a 2D image
                                         -- object, the depth value given must
                                         -- be 1.
                              -> a -- ^ The offset where to begin copying data
                                   -- into dst_buffer. The size in bytes of the
                                   -- region to be copied referred to as dst_cb
                                   -- is computed as width * height * depth *
                                   -- bytes/image element if src_image is a 3D
                                   -- image object and is computed as width *
                                   -- height * bytes/image element if src_image
                                   -- is a 2D image object.
                              -> [CLEvent] -- ^ Specify events that need to
                                           -- complete before this particular
                                           -- command can be executed. If
                                           -- event_wait_list is empty, then
                                           -- this particular command does not
                                           -- wait on any event to complete. The
                                           -- events specified in
                                           -- event_wait_list act as
                                           -- synchronization points. The
                                           -- context associated with events in
                                           -- event_wait_list and command_queue
                                           -- must be the same.
                              -> IO CLEvent
clEnqueueCopyImageToBuffer lib cq src dst (src_orix,src_oriy,src_oriz) (regx,regy,regz) offset xs =
  withArray (fmap fromIntegral [src_orix,src_oriy,src_oriz]) $ \psrc_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (rawClEnqueueCopyImageToBuffer lib cq src dst psrc_ori preg (fromIntegral offset)) xs

clEnqueueCopyBufferToImage :: Integral a 
                              => OpenCLLibrary 
                              -> CLCommandQueue -- ^ The OpenCL context
                                                -- associated with
                                                -- command_queue, src_image, and
                                                -- dst_buffer must be the same.
                              -> CLMem -- ^ src. A valid buffer object.
                              -> CLMem -- ^ dst. A valid image object.
                              -> a -- ^ The offset where to begin copying data
                                   -- from src_buffer.
                              -> (a,a,a) -- ^ The (x, y, z) offset in pixels
                                         -- where to begin copying data to
                                         -- dst_image. If dst_image is a 2D
                                         -- image object, the z value given by
                                         -- must be 0.
                              -> (a,a,a) -- ^ Defines the (width, height, depth)
                                         -- in pixels of the 2D or 3D rectangle
                                         -- to copy. If dst_image is a 2D image
                                         -- object, the depth value given by
                                         -- must be 1.
                              -> [CLEvent] -- ^ Specify events that need to
                                           -- complete before this particular
                                           -- command can be executed. If
                                           -- event_wait_list is empty, then
                                           -- this particular command does not
                                           -- wait on any event to complete. The
                                           -- events specified in
                                           -- event_wait_list act as
                                           -- synchronization points. The
                                           -- context associated with events in
                                           -- event_wait_list and command_queue
                                           -- must be the same.
                              -> IO CLEvent
clEnqueueCopyBufferToImage lib cq src dst offset (dst_orix,dst_oriy,dst_oriz) (regx,regy,regz) xs =
  withArray (fmap fromIntegral [dst_orix,dst_oriy,dst_oriz]) $ \pdst_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (rawClEnqueueCopyBufferToImage lib cq src dst (fromIntegral offset) pdst_ori preg) xs

clEnqueueMapBuffer :: Integral a => OpenCLLibrary 
                      -> CLCommandQueue 
                      -> CLMem -- ^ A valid buffer object. The OpenCL context
                               -- associated with command_queue and buffer must
                               -- be the same.
                      -> Bool -- ^ Indicates if the map operation is blocking or
                              -- non-blocking.
                      -> [CLMapFlag] -- ^ Is a list and can be set to
                                     -- 'CL_MAP_READ' to indicate that the
                                     -- region specified by (offset, cb) in the
                                     -- buffer object is being mapped for
                                     -- reading, and/or 'CL_MAP_WRITE' to
                                     -- indicate that the region specified by
                                     -- (offset, cb) in the buffer object is
                                     -- being mapped for writing.
                      -> a -- ^ The offset in bytes of the region in the buffer
                           -- object that is being mapped.
                      -> a -- ^ The size in bytes of the region in the buffer
                           -- object that is being mapped.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty,
                                   -- then this particular command does not wait
                                   -- on any event to complete. The events
                                   -- specified in event_wait_list act as
                                   -- synchronization points. The context
                                   -- associated with events in event_wait_list
                                   -- and command_queue must be the same.

                      -> IO (CLEvent, Ptr ())
clEnqueueMapBuffer lib cq mem check xs offset cb [] = 
  alloca $ \pevent -> do
    val <- wrapPError $ \perr -> rawClEnqueueMapBuffer lib cq mem (fromBool check) flags (fromIntegral offset) (fromIntegral cb) 0 nullPtr pevent perr
    event <- peek pevent
    return (event, val)
    
      where
        flags = bitmaskFromFlags xs
clEnqueueMapBuffer lib cq mem check xs offset cb events = 
  allocaArray nevents $ \pevents -> do
    pokeArray pevents events
    alloca $ \pevent -> do
      val <- wrapPError $ \perr -> rawClEnqueueMapBuffer lib cq mem (fromBool check) flags (fromIntegral offset) (fromIntegral cb) cnevents pevents pevent perr
      event <- peek pevent
      return (event, val)
    where
      flags = bitmaskFromFlags xs
      nevents = length events
      cnevents = fromIntegral nevents

clEnqueueMapImage :: Integral a => OpenCLLibrary 
                     -> CLCommandQueue 
                     -> CLMem -- ^ A valid image object. The OpenCL context
                              -- associated with command_queue and image must be
                              -- the same.
                     -> Bool -- ^ Indicates if the map operation is blocking or
                             -- non-blocking. If blocking_map is 'True',
                             -- 'clEnqueueMapImage' does not return until the
                             -- specified region in image can be mapped.
                     -> [CLMapFlag] -- ^ Is a bit-field and can be set to
                                    -- 'CL_MAP_READ' to indicate that the region
                                    -- specified by (origin, region) in the
                                    -- image object is being mapped for reading,
                                    -- and/or 'CL_MAP_WRITE' to indicate that the
                                    -- region specified by (origin, region) in
                                    -- the image object is being mapped for
                                    -- writing.
                     -> (a,a,a) -- ^ Define the (x, y, z) offset in pixels of
                                -- the 2D or 3D rectangle region that is to be
                                -- mapped. If image is a 2D image object, the z
                                -- value given must be 0.
                     -> (a,a,a) -- ^ Define the (width, height, depth) in pixels
                                -- of the 2D or 3D rectangle region that is to
                                -- be mapped. If image is a 2D image object, the
                                -- depth value given must be 1.
                     -> [CLEvent] -- ^ Specify events that need to complete
                                  -- before 'clEnqueueMapImage' can be
                                  -- executed. If event_wait_list is empty, then
                                  -- 'clEnqueueMapImage' does not wait on any
                                  -- event to complete. The events specified in
                                  -- event_wait_list act as synchronization
                                  -- points. The context associated with events
                                  -- in event_wait_list and command_queue must
                                  -- be the same.
                     -> IO (CLEvent, (Ptr (), CSize, CSize))
clEnqueueMapImage lib cq mem check xs (orix,oriy,oriz) (regx,regy,regz) [] = 
  alloca $ \ppitch -> 
  alloca $ \pslice ->
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  alloca $ \pevent -> do
    val <- wrapPError $ \perr -> rawClEnqueueMapImage lib cq mem (fromBool check) flags pori preg ppitch pslice 0 nullPtr pevent perr
    event <- peek pevent
    pitch <- peek ppitch
    slice <- peek pslice
    return (event, (val, pitch, slice))
    
      where
        flags = bitmaskFromFlags xs
clEnqueueMapImage lib cq mem check xs (orix,oriy,oriz) (regx,regy,regz) events = 
  alloca $ \ppitch -> 
  alloca $ \pslice ->
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  allocaArray nevents $ \pevents -> do
    pokeArray pevents events
    alloca $ \pevent -> do
      val <- wrapPError $ \perr -> rawClEnqueueMapImage lib cq mem (fromBool check) flags pori preg ppitch pslice cnevents pevents pevent perr
      event <- peek pevent
      pitch <- peek ppitch
      slice <- peek pslice
      return (event, (val, pitch, slice))

    where
      flags = bitmaskFromFlags xs
      nevents = length events
      cnevents = fromIntegral nevents
      
clEnqueueUnmapMemObject :: OpenCLLibrary 
                           -> CLCommandQueue 
                           -> CLMem -- ^ A valid memory object. The OpenCL
                                    -- context associated with command_queue and
                                    -- memobj must be the same.
                           -> Ptr () -- ^ The host address returned by a
                                     -- previous call to 'clEnqueueMapBuffer' or
                                     -- 'clEnqueueMapImage' for memobj.
                           -> [CLEvent] -- ^ Specify events that need to
                                        -- complete before
                                        -- 'clEnqueueUnmapMemObject' can be
                                        -- executed. If event_wait_list is
                                        -- empty, then 'clEnqueueUnmapMemObject'
                                        -- does not wait on any event to
                                        -- complete. The events specified in
                                        -- event_wait_list act as
                                        -- synchronization points. The context
                                        -- associated with events in
                                        -- event_wait_list and command_queue
                                        -- must be the same.

                           -> IO CLEvent
clEnqueueUnmapMemObject lib cq mem pp = clEnqueue (rawClEnqueueUnmapMemObject lib cq mem pp)

clEnqueueNDRangeKernel :: Integral a => OpenCLLibrary -> CLCommandQueue -> CLKernel -> [a] -> [a] 
                          -> [CLEvent] -> IO CLEvent
clEnqueueNDRangeKernel lib cq krn gws lws events = withArray (map fromIntegral gws) $ \pgws -> withMaybeArray (map fromIntegral lws) $ \plws -> do
  clEnqueue (rawClEnqueueNDRangeKernel lib cq krn num nullPtr pgws plws) events
    where
      num = fromIntegral $ length gws

clEnqueueTask :: OpenCLLibrary -> CLCommandQueue -> CLKernel -> [CLEvent] -> IO CLEvent
clEnqueueTask lib cq krn = clEnqueue (rawClEnqueueTask lib cq krn)
  
clEnqueueNativeKernel :: OpenCLLibrary -> CLCommandQueue -> (Ptr () -> IO ()) -> Ptr () -> CSize 
                         -> [CLMem] -> [Ptr ()] -> [CLEvent] -> IO CLEvent
clEnqueueNativeKernel lib cq f dat sz xs ys evs = 
  withMaybeArray xs $ \pmem -> 
  withMaybeArray ys $ \pbuff -> do
    fptr <- wrapNativeKernelCallback f
    clEnqueue (rawClEnqueueNativeKernel lib cq fptr dat sz 
               (fromIntegral . length $ xs) pmem pbuff) evs
                          
clEnqueueMarker :: OpenCLLibrary -> CLCommandQueue -> IO CLEvent
clEnqueueMarker lib cq = alloca $ \event 
                              -> whenSuccess (rawClEnqueueMarker lib cq event)
                                 $ peek event
         
clEnqueueWaitForEvents :: OpenCLLibrary -> CLCommandQueue -> [CLEvent] -> IO ()
clEnqueueWaitForEvents lib cq [] = whenSuccess 
                               (rawClEnqueueWaitForEvents lib cq 0 nullPtr)
                               $ return ()
clEnqueueWaitForEvents lib cq events = allocaArray nevents $ \pevents -> do
  pokeArray pevents events
  whenSuccess (rawClEnqueueWaitForEvents lib cq cnevents pevents)
    $ return ()
    where
      nevents = length events
      cnevents = fromIntegral nevents

clEnqueueBarrier :: OpenCLLibrary -> CLCommandQueue -> IO ()
clEnqueueBarrier lib cq = whenSuccess (rawClEnqueueBarrier lib cq) $ return ()
  
clFlush :: OpenCLLibrary -> CLCommandQueue -> IO Bool
clFlush lib = wrapCheckSuccess . (rawClFlush lib)
             
clFinish :: OpenCLLibrary -> CLCommandQueue -> IO Bool
clFinish lib = wrapCheckSuccess . (rawClFinish lib)
