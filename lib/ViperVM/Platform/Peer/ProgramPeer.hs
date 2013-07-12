module ViperVM.Platform.Peer.ProgramPeer (
   ProgramPeer(..), programCompile
) where

import qualified ViperVM.Backends.OpenCL.Processor as CL
import qualified ViperVM.Backends.OpenCL.Program as CL
import qualified ViperVM.Platform.Peer.ProcPeer as Peer

import ViperVM.Platform.Compilation

data ProgramPeer = CLProgram CL.CLProgram

-- | COmpile a program for the given procs
programCompile :: ProgramPeer -> [Peer.ProcPeer] -> IO [CompilationResult]
programCompile p procs = case p of
   CLProgram peer -> CL.programCompile peer (filterCLProcs procs)

   where
      filterCLProcs [] = []
      filterCLProcs (Peer.CLProc p':ps) = p':(filterCLProcs ps)
      --filterCLProcs (_:ps) = filterCLProcs ps
