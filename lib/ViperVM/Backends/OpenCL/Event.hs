{-# LANGUAGE ScopedTypeVariables #-}
module ViperVM.Backends.OpenCL.Event(  
  -- * Types
  CLEvent, CLCommandType(..), CLProfilingInfo(..), CLCommandExecutionStatus(..),
  -- * Functions
  clWaitForEvents, clRetainEvent, clReleaseEvent, clGetEventCommandQueue, 
  clGetEventCommandType, clGetEventReferenceCount, 
  clGetEventCommandExecutionStatus, clGetEventProfilingInfo
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Backends.OpenCL.Types( 
  CLEvent, CLint, CLulong,
  CLCommandQueue, CLCommandType(..), CLCommandType_, 
  CLCommandExecutionStatus(..), CLProfilingInfo(..), getCommandExecutionStatus, 
  getCLValue, getEnumCL, wrapCheckSuccess, wrapGetInfo )

-- -----------------------------------------------------------------------------
-- | Waits on the host thread for commands identified by event objects in 
-- event_list to complete. A command is considered complete if its execution 
-- status is 'CL_COMPLETE' or a negative value.
-- Returns 'True' if the function was executed successfully. It returns 'False'
-- if the list of events is empty, or if events specified in event_list do not 
-- belong to the same context, or if event objects specified in event_list are 
-- not valid event objects.
clWaitForEvents :: OpenCLLibrary -> [CLEvent] -> IO Bool
clWaitForEvents _ [] = return False
clWaitForEvents lib xs = allocaArray nevents $ \pevents -> do
  pokeArray pevents xs
  wrapCheckSuccess $ rawClWaitForEvents lib (fromIntegral nevents) pevents
    where
      nevents = length xs
  
-- | Increments the event reference count.
-- The OpenCL commands that return an event perform an implicit retain.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clRetainEvent :: OpenCLLibrary -> CLEvent -> IO Bool
clRetainEvent lib ev = wrapCheckSuccess $ rawClRetainEvent lib ev

-- | Decrements the event reference count.
-- Decrements the event reference count. The event object is deleted once the 
-- reference count becomes zero, the specific command identified by this event 
-- has completed (or terminated) and there are no commands in the command-queues 
-- of a context that require a wait for this event to complete.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clReleaseEvent :: OpenCLLibrary -> CLEvent -> IO Bool
clReleaseEvent lib ev = wrapCheckSuccess $ rawClReleaseEvent lib ev

data CLEventInfo = 
     CL_EVENT_COMMAND_QUEUE
   | CL_EVENT_COMMAND_TYPE
   | CL_EVENT_COMMAND_EXECUTION_STATUS
   | CL_EVENT_REFERENCE_COUNT
   | CL_EVENT_CONTEXT

instance Enum CLEventInfo where
   fromEnum CL_EVENT_COMMAND_QUEUE              = 0x11D0
   fromEnum CL_EVENT_COMMAND_TYPE               = 0x11D1
   fromEnum CL_EVENT_COMMAND_EXECUTION_STATUS   = 0x11D2
   fromEnum CL_EVENT_REFERENCE_COUNT            = 0x11D3
   fromEnum CL_EVENT_CONTEXT                    = 0x11D4

   toEnum 0x11D0 = CL_EVENT_COMMAND_QUEUE
   toEnum 0x11D1 = CL_EVENT_COMMAND_TYPE
   toEnum 0x11D2 = CL_EVENT_COMMAND_EXECUTION_STATUS
   toEnum 0x11D3 = CL_EVENT_REFERENCE_COUNT
   toEnum 0x11D4 = CL_EVENT_CONTEXT
   toEnum _ = error "Invalid Event Info value"

-- | Return the command-queue associated with event.
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_COMMAND_QUEUE'.
clGetEventCommandQueue :: OpenCLLibrary -> CLEvent -> IO CLCommandQueue
clGetEventCommandQueue lib ev =
    wrapGetInfo (\(dat :: Ptr CLCommandQueue) ->
        rawClGetEventInfo lib ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_EVENT_COMMAND_QUEUE
      size = fromIntegral $ sizeOf (nullPtr::CLCommandQueue)
      
-- | Return the command associated with event.
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_COMMAND_TYPE'.
clGetEventCommandType :: OpenCLLibrary -> CLEvent -> IO CLCommandType
clGetEventCommandType lib ev =
    wrapGetInfo (\(dat :: Ptr CLCommandType_) ->
        rawClGetEventInfo lib ev infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_EVENT_COMMAND_TYPE
      size = fromIntegral $ sizeOf (0::CLCommandType_)
      
-- | Return the event reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in applications. 
-- This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_REFERENCE_COUNT'.
clGetEventReferenceCount :: OpenCLLibrary -> CLEvent -> IO CLint
clGetEventReferenceCount lib ev =
    wrapGetInfo (\(dat :: Ptr CLint) ->
        rawClGetEventInfo lib ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_EVENT_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLint)

-- | Return the execution status of the command identified by event.
--
-- This function execute OpenCL clGetEventInfo with
-- 'CL_EVENT_COMMAND_EXECUTION_STATUS'.
clGetEventCommandExecutionStatus :: OpenCLLibrary -> CLEvent -> IO CLCommandExecutionStatus
clGetEventCommandExecutionStatus lib ev =
    wrapGetInfo (\(dat :: Ptr CLint) ->
        rawClGetEventInfo lib ev infoid size (castPtr dat)) getCommandExecutionStatus
    where 
      infoid = getCLValue CL_EVENT_COMMAND_EXECUTION_STATUS
      size = fromIntegral $ sizeOf (0::CLint)
      
clGetEventProfilingInfo :: OpenCLLibrary -> CLEvent -> CLProfilingInfo -> IO CLulong
clGetEventProfilingInfo lib ev prof =
    wrapGetInfo (\(dat :: Ptr CLulong) ->
        rawClGetEventProfilingInfo lib ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue prof
      size = fromIntegral $ sizeOf (0::CLulong)
