module ViperVM.Platform.KernelExecutionResult where

data ExecutionResult = ExecuteSuccess 
                     | ExecuteError
                       deriving (Eq)

