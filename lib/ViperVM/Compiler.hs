module ViperVM.Compiler (
  initSingleCompiler,
  Compiler(..)
  ) where

import ViperVM.Kernel
import ViperVM.Platform
import ViperVM.Logging.Logger

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad (unless,void)

data CompilerMessage = Compile [Processor] Kernel ([Maybe CompiledKernel] -> IO ()) |
                       Quit (IO ())

data Compiler = Compiler {
  compile :: [Processor] -> Kernel -> ([Maybe CompiledKernel] -> IO ()) -> IO (),
  shutdown :: IO () -> IO ()
}

initSingleCompiler :: Logger -> IO Compiler
initSingleCompiler logger = do
  ch <- newChan
  void $ forkIO $ compilerThreadLoop logger ch
  return $ Compiler (compile' ch) (quit' ch)

compile' :: Chan CompilerMessage -> [Processor] -> Kernel -> ([Maybe CompiledKernel] -> IO ()) -> IO ()
compile' ch ps k cb = writeChan ch $ Compile ps k cb

quit' :: Chan CompilerMessage -> IO () -> IO ()
quit' ch cb = writeChan ch $ Quit cb

-- | True if message is Quit
isQuit :: CompilerMessage -> Bool
isQuit (Quit _) = True
isQuit _ = False

compilerThreadLoop :: Logger -> Chan CompilerMessage -> IO ()
compilerThreadLoop logger ch = do
  msg <- readChan ch
  
  case msg of
    Compile procs k cb -> do
      (ck,ta) <- timeAction $ compileKernels k procs
      logger $ KernelCompilation k procs ck ta
      cb ck
      
    Quit cb -> cb

  unless (isQuit msg) $ compilerThreadLoop logger ch
