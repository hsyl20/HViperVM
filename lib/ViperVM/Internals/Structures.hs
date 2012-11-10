{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.Internals.Structures where

import ViperVM.Buffer
import ViperVM.Data
import ViperVM.Event
import ViperVM.Kernel
import ViperVM.KernelSet
import ViperVM.Logging.Logger
import ViperVM.Platform
import ViperVM.Task
import ViperVM.Transfer

import Control.Concurrent.Chan
import Control.Monad.State
import Data.Lens.Lazy
import Data.Lens.Template
import Data.Map (Map)
import Data.Set (Set)
import Data.Word
import Foreign.Ptr
import qualified Data.Map as Map

-- | Messages that the runtime can handle.
data Message =  
    AppTaskSubmit KernelSet [Data] (Event [Data])-- ^ A task has been submitted by the application
  | AppQuit (Event ())            -- ^ Runtime shutdown is requested
  | AppMapVector DataDesc (Ptr ()) (Event Data) -- ^ A vector data is to be created using existing data
  | TaskSubmitted Task            -- ^ A task has been submitted
  | TaskScheduled Task Processor  -- ^ A task has been scheduled on a given processor
  | TaskComplete Task             -- ^ A task has completed
  | TaskReady Task                -- ^ A task has no associated request left
  | RequestsStored                -- ^ Some new requests have been submitted
  | KernelComplete Kernel         -- ^ A kernel has completed
  | KernelCompiled Kernel [Processor] [Maybe CompiledKernel] -- ^ A kernel compilation has completed
  | DataAllocated Data DataInstance -- ^ A placeholder has been allocated for the given data
  | DataTransfered Data DataInstance -- ^ A data has been transfered or duplicated
  | DataComputed Data             -- ^ A data has been computed

-- | Requests that are associated to tasks and that must be fulfilled before the task cen be executed
data TaskRequest = 
   RequestComputation Data                -- ^ Request that a data has been computed
 | RequestCompilation [Kernel] Processor  -- ^ Request compilation of at least one kernel for the given processor
 | RequestTransfer [Memory] Data          -- ^ Request the transfer of a data instance in any of the given memories
 | RequestDuplication [Memory] Data       -- ^ Request a duplicated instance (i.e. detachable) of the data in any of the given memories
 | RequestAllocation [Memory] Data        -- ^ Request the allocation of a placeholder for the given data in any of the given memories
 deriving (Eq,Ord,Show)

-- | State of the runtime system
data RuntimeState = RuntimeState {
  channel :: Chan Message,                  -- ^ Channel to communicate with the runtime
  platform :: Platform,                     -- ^ Platform used by the runtime
  logger :: Logger,                         -- ^ Logging method
  scheduler :: Scheduler,                   -- ^ Scheduler
  linkChannels :: Map Link (Chan Transfer), -- ^ Channels to communicate with link threads
  -- Lenses
  _buffers :: [Buffer],                     -- ^ Associate buffer ID to real buffers
  _memBuffers :: Map Memory [Buffer],       -- ^ Buffers in each memory
  _datas :: Map Data [DataInstance],        -- ^ Data and their valid instances
  _dataTasks :: Map Data Task,              -- ^ Task computing each (uncomputed) data
  _dataCounter :: Word,                     -- ^ Data counter (used to set data ID)
  _bufferCounter :: Word,                   -- ^ Buffer counter (used to set buffer ID)
  _invalidDataInstances :: Map Data [DataInstance], -- ^ Data and their invalid instances (just allocated, used in a transfer, etc.)
  _compiledKernels :: Map Kernel (Map Processor CompiledKernel), -- ^ Compiled kernel cache
  _submittedTasks :: [Task],                -- ^ Tasks that are to be scheduled
  _scheduledTasks :: Map Processor [Task],  -- ^ Tasks scheduled on processors (may be executing)
  _requestTasks :: Map TaskRequest [Task],  -- ^ Requests and tasks that have made the request
  _taskRequests :: Map Task (Set TaskRequest),  -- ^ Tasks and the request the have made
  _activeRequests :: Set TaskRequest
}

type R = StateT RuntimeState IO
type Scheduler = Message -> R ()

newtype Runtime = Runtime (Chan Message)

$( makeLens ''RuntimeState )


-- | Execute an IO action using the runtime state
withStateR :: (RuntimeState -> IO a) -> R a
withStateR f = do
  st <- get
  lift $ f st

-- | Execute an IO action using the runtime state and drop the result
withStateR_ :: (RuntimeState -> IO a) -> R ()
withStateR_ f = do
  st <- get
  lift $ void $ f st

-- | True if message is Quit
isQuit :: Message -> Bool
isQuit (AppQuit _) = True
isQuit _ = False

-- | Post a message on the message channel
postMessageR :: Message -> R ()
postMessageR msg = withStateR_ $ flip writeChan msg . channel

-- | "Do nothing" in the R monad
voidR :: R ()
voidR = lift $ return ()

----------------------------------------------------------
-- Accessors
----------------------------------------------------------

-- | Return processors of the platform
getProcessorsR :: R [Processor]
getProcessorsR = gets (processors . platform)

-- | Return the logger
getLoggerR :: R Logger
getLoggerR = gets logger

-- | Return the message channel
getChannelR :: R (Chan Message)
getChannelR = gets channel

-- | Return compiled kernels
getCompiledKernelsR :: R (Map Kernel (Map Processor CompiledKernel))
getCompiledKernelsR = gets (compiledKernels ^$)

-- | Return a compiled kernel from the cache, if any
getCompiledKernelR :: Processor -> Kernel -> R (Maybe CompiledKernel)
getCompiledKernelR p k = gets (getCompiledKernel p k . getL compiledKernels)

getCompiledKernel :: Processor -> Kernel -> Map Kernel (Map Processor CompiledKernel) -> Maybe CompiledKernel
getCompiledKernel p k cks = Map.lookup k cks >>= Map.lookup p

-- | Store a compiled kernel in state
storeCompiledKernelR :: Kernel -> CompiledKernel -> Processor -> R ()
storeCompiledKernelR k ck proc = modify $ compiledKernels ^%= Map.insertWith Map.union k (Map.singleton proc ck)


