{-# LANGUAGE TemplateHaskell, FlexibleContexts #-} 

module ViperVM.MemoryManager (
   MemoryManager, AccessMode(..),
   BufferAllocation(..), BufferRelease(..), RegionLock(..), RegionUnlock(..), RegionTransfer(..),
   BufferReleaseState(..),
   initMemoryManager, shutdown, allocateBuffer, releaseBuffer, lockRegion, unlockRegion, transferRegion,
) where

import qualified ViperVM.BufferManager as BufferManager
import qualified ViperVM.RegionManager as RegionManager
import qualified ViperVM.TransferManager as TransferManager
import ViperVM.Buffer
import ViperVM.BufferManager 
import ViperVM.Event
import ViperVM.Platform
import ViperVM.Region
import ViperVM.RegionManager 
import ViperVM.TransferManager 

import Data.Word
import Data.Lens.Lazy
import Data.Lens.Template

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Control.Applicative

-- | Manage memory allocations, releases and transfers
data MemoryManager = MemoryManager {
      channel :: Chan Command,
      platform :: Platform,
      _bufferManager :: BufferManager,
      _regionManager :: RegionManager,
      _transferManager :: TransferManager
   }



-- Commands
class Cmd a where
   wrap :: a -> Command

data AccessMode = ReadOnly | ReadWrite | WriteOnly

data Shutdown = Shutdown (Event ())
data BufferAllocation = BufferAllocation Memory Word64 (Event (Maybe Buffer))
data BufferRelease = BufferRelease Buffer (Event BufferReleaseState)
data RegionLock = RegionLock Buffer Region AccessMode (Event Bool)
data RegionUnlock = RegionUnlock Buffer Region AccessMode (Event Bool)
data RegionTransfer = RegionTransfer Buffer Region Buffer Region (Event Bool)

data Command = ShutdownCmd Shutdown |
               BufferAllocationCmd BufferAllocation |
               BufferReleaseCmd BufferRelease |
               RegionLockCmd RegionLock |
               RegionUnlockCmd RegionUnlock |
               RegionTransferCmd RegionTransfer

instance Cmd Shutdown where wrap = ShutdownCmd
instance Cmd BufferAllocation where wrap = BufferAllocationCmd
instance Cmd BufferRelease where wrap = BufferReleaseCmd
instance Cmd RegionLock where wrap = RegionLockCmd
instance Cmd RegionUnlock where wrap = RegionUnlockCmd
instance Cmd RegionTransfer where wrap = RegionTransferCmd


data BufferReleaseState = BufferReleaseSuccess | BufferReleaseErrorRegionLeft
                          deriving (Show)

$( makeLens ''MemoryManager )


-----------------------------------------------------------------------
-- User interface (mostly asynchronous)
-----------------------------------------------------------------------

-- | Initialize memory manager
initMemoryManager :: Platform -> IO MemoryManager
initMemoryManager pf = do
   ch <- newChan

   let mgr = MemoryManager {
      channel = ch,
      platform = pf,
      _bufferManager = BufferManager.init,
      _regionManager = RegionManager.init,
      _transferManager = TransferManager.init
   }

   void $ forkIO (memoryManager mgr)

   return mgr



-- | Command creation and submission helper
createCommand :: Cmd a => MemoryManager -> (Event b -> a) -> IO a
createCommand mgr f = do
   cmd <- f . publishEvent <$> newEvent
   writeChan (channel mgr) (wrap cmd)
   return cmd

-- | Try to shutdown the manager
shutdown :: MemoryManager -> IO Shutdown
shutdown mgr = createCommand mgr Shutdown

-- | Try to allocate a buffer of the specified size in the given memory
allocateBuffer :: MemoryManager -> Memory -> Word64 -> IO BufferAllocation
allocateBuffer mgr mem sz = createCommand mgr (BufferAllocation mem sz)

-- | Try to release a buffer
releaseBuffer :: MemoryManager -> Buffer -> IO BufferRelease
releaseBuffer mgr buffer = createCommand mgr (BufferRelease buffer)

-- | Try to lock a region
lockRegion :: MemoryManager -> Buffer -> Region -> AccessMode -> IO RegionLock
lockRegion mgr buffer region mode = createCommand mgr (RegionLock buffer region mode)

-- | Try to unlock a region
unlockRegion :: MemoryManager -> Buffer -> Region -> AccessMode -> IO RegionUnlock
unlockRegion mgr buffer region mode = createCommand mgr (RegionUnlock buffer region mode)

-- | Try to transfer a region
transferRegion :: MemoryManager -> Buffer -> Region -> Buffer -> Region -> IO RegionTransfer
transferRegion mgr buf1 reg1 buf2 reg2  = createCommand mgr (RegionTransfer buf1 reg1 buf2 reg2 )


-----------------------------------------------------------------------
-- Memory Manager thread
-----------------------------------------------------------------------

memoryManager :: MemoryManager -> IO ()
memoryManager mgr = do
   cmd <- readChan (channel mgr)

   let execLoop d = executeCommand mgr d >>= memoryManager

   case cmd of 
      ShutdownCmd {} -> return ()
      _ -> execLoop cmd


-- | Effective command execution is performed here
executeCommand :: MemoryManager -> Command -> IO MemoryManager

executeCommand _ (ShutdownCmd {}) = undefined

-- Buffer allocation
executeCommand mgr (BufferAllocationCmd (BufferAllocation mem sz ev)) = do
   (bufMgr2, buffer) <- allocate (bufferManager ^$ mgr) mem sz
   setEvent ev buffer
   return (bufferManager ^= bufMgr2 $ mgr) 

-- Buffer release
executeCommand mgr (BufferReleaseCmd (BufferRelease buffer ev)) = do

   -- Check that there is no region left in the buffer
   let regs = regions (regionManager ^$ mgr) buffer
   if not (null regs) 
      then do
         setEvent ev BufferReleaseErrorRegionLeft
         return mgr
      else do
         bufMgr2 <- free (bufferManager ^$ mgr) buffer
         setEvent ev BufferReleaseSuccess
         return (bufferManager ^= bufMgr2 $ mgr) 
