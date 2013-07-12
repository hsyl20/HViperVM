{-# LANGUAGE DeriveDataTypeable #-}
module ViperVM.Platform.Memory (
   Memory(..), memoryName, memoryInfo, wrapMemory,
   Buffer(..), bufferSize,
   bufferAllocate, bufferRelease
) where

import qualified ViperVM.Platform.MemoryPeer as Peer

import ViperVM.STM.TSet
import qualified ViperVM.STM.TSet as TSet

import Text.Printf
import Data.Typeable
import Data.Word
import Data.Traversable (forM)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent.STM (atomically,TVar,newTVarIO,writeTVar,readTVar)

--------------------------------------------
-- Memory
--------------------------------------------

-- | A memory
data Memory = Memory {
   memoryId :: Int,
   memoryPeer :: Peer.MemoryPeer,
   memoryBuffers :: TSet Buffer,
   memoryNextBufferId :: TVar Int
}

instance Show Memory where
   show m = "Memory " ++ show (memoryId m)

instance Eq Memory where
   (==) m1 m2 = memoryId m1 == memoryId m2

instance Ord Memory where
   compare m1 m2 = compare (memoryId m1) (memoryId m2)

-- | Wrap a peer memory
wrapMemory :: Peer.MemoryPeer -> Int -> IO Memory
wrapMemory peer memId = Memory memId peer <$> atomically TSet.empty <*> newTVarIO 0

-- | Retrieve memory name
memoryName :: Memory -> String
memoryName = Peer.memoryName . memoryPeer

-- | Retrieve memory size
memorySize :: Memory -> Word64
memorySize = Peer.memorySize . memoryPeer

-- | Retrieve memory information string
memoryInfo :: Memory -> String
memoryInfo m = printf "%s - %s" (memoryName m) (prettyMemorySize m)

-- | Pretty print memory size
prettyMemorySize :: Memory -> String
prettyMemorySize m = prettyShowSize (fromIntegral (memorySize m)) Base

data Unit = Base | Kilo | Mega | Giga | Tera | Peta 

instance Show Unit where
   show Base = "bytes"
   show Kilo = "kB"
   show Mega = "MB"
   show Giga = "GB"
   show Tera = "TB"
   show Peta = "PB"

-- | Pretty print a size
prettyShowSize :: Double -> Unit -> String
prettyShowSize n unit = if n > 1023 then prettyShowSize (n / 1024.0) (nextUnit unit) else printf "%.2f %s" n (show unit)
   where
      nextUnit :: Unit -> Unit
      nextUnit u = case u of 
         Base -> Kilo
         Kilo -> Mega
         Mega -> Giga
         Giga -> Tera
         Tera -> Peta
         Peta -> undefined

--------------------------------------------
-- Buffer
--------------------------------------------

-- | A buffer allocated in a memory
data Buffer = Buffer {
   bufferLocalId :: Int,  -- buffer identifier in the memory (not globally)
   bufferPeer :: Peer.BufferPeer,
   bufferMemory :: Memory
} deriving (Typeable)

-- | Global buffer identifier
bufferId :: Buffer -> (Int,Int)
bufferId b = (memoryId (bufferMemory b), bufferLocalId b)

instance Eq Buffer where
   (==) b1 b2 = bufferId b1 == bufferId b2

instance Ord Buffer where
   compare b1 b2 = compare (bufferId b1) (bufferId b2)

instance Show Buffer where
   show b = "Buffer " ++ show (bufferId b)

-- | Wrap a peer buffer
wrapBuffer :: Int -> Peer.BufferPeer -> Memory -> Buffer
wrapBuffer bId peer mem = Buffer bId peer mem

-- | Return the size of the allocated buffer
bufferSize :: Buffer -> Word64
bufferSize = Peer.bufferSize . bufferPeer
              
-- | Try to allocate a buffer in a memory
bufferAllocate :: Word64 -> Memory -> IO (Maybe Buffer)
bufferAllocate sz m = do

   -- Peer allocate
   peerBuf <- Peer.bufferAllocate sz (memoryPeer m)

   -- If allocation is successful, wrap buffer
   forM peerBuf $ \b -> atomically $ do

      -- Get buffer ID and increase for the next one
      bId <- readTVar (memoryNextBufferId m)
      writeTVar (memoryNextBufferId m) (bId + 1)

      -- Wrap buffer
      let buf = wrapBuffer bId b m

      -- Add the buffer to the memory buffer list
      TSet.insert buf (memoryBuffers m)

      return buf



-- | Release a buffer
bufferRelease :: Buffer -> IO ()
bufferRelease b = do
   let m = bufferMemory b

   -- Remove the buffer from memory list of buffers
   atomically $ do
      TSet.delete b (memoryBuffers m)

   -- Peer release
   Peer.bufferRelease (bufferPeer b)

