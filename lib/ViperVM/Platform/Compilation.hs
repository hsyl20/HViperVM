module ViperVM.Platform.Compilation where

data CompilationResult = CompilationSuccess
                       | CompilationFailure String

isCompilationSuccess :: CompilationResult -> Bool
isCompilationSuccess (CompilationSuccess {}) = True
isCompilationSuccess _ = False

