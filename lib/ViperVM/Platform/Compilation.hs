module ViperVM.Platform.Compilation where

import qualified ViperVM.Backends.OpenCL.Kernel as CL

data CompilationResult = CompilationSuccess CompiledKernel
                       | CompilationFailure String

data PeerCompiledKernel = CLCompiledKernel CL.CompiledKernel

data CompiledKernel = CompiledKernel {
   peerCompiledKernel :: PeerCompiledKernel
}

isCompilationSuccess :: CompilationResult -> Bool
isCompilationSuccess (CompilationSuccess {}) = True
isCompilationSuccess _ = False

