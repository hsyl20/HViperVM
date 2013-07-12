module ViperVM.Platform.Program (
   Program(..), programCompile
) where

import ViperVM.STM.TMap
import ViperVM.Platform.Proc
import ViperVM.Platform.Compilation
import qualified ViperVM.Platform.Peer.ProgramPeer as Peer

import Control.Concurrent.STM
import qualified Data.Map as Map

-- | A compiled source
data Program = Program {
   programPeer :: Peer.ProgramPeer,
   programs :: TMap Proc CompilationResult
}

-- | Compile program for the given procs
programCompile :: Program -> [Proc] -> IO ()
programCompile prog procs = do
   res <- Peer.programCompile (programPeer prog) (fmap procPeer procs)

   let cs2 = Map.fromList (procs `zip` res)

   atomically $ do
      cs <- readTVar (programs prog)
      writeTVar (programs prog) (Map.union cs cs2)
