import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionManager
import ViperVM.Platform.BufferManager (createBufferManager)

import ViperVM.Library.FloatMatrixAdd
import ViperVM.Platform.Primitive as Prim

import Control.Monad

main :: IO ()
main = do
   let config = Configuration {
      libraryOpenCL = "/usr/lib/libOpenCL.so"
   }

   putStrLn "Initializing platform..."
   platform <- initPlatform config

   bm <- createBufferManager platform
   rm <- createRegionManager bm
   km <- createKernelManager rm

   let (w,h) = (1024, 512)
       padding = 11
       bufferSize = (w + padding) * h * (Prim.size Prim.Float) 
       openclProcs = filter isOpenCLProcessor (processors platform)
       ker = floatMatrixAddCL
       reg = Region2D 0 h w padding

   putStrLn "Registering kernel..." 
   registerKernel km ker

   putStrLn "OpenCL processors:"
   forM_ openclProcs (putStrLn . show)

   putStrLn "\nCompiling kernel..." 
   validProcs <- compileKernel km ker openclProcs

   putStrLn "Compilation succeeded for processors:" 
   forM_ validProcs (putStrLn . show)
   putStrLn ""

   forM_ validProcs $ \proc -> do

      putStrLn ("Executing kernel on " ++ show proc)
      let mem = head (processorMemories proc)

      Just a <- allocateBuffer rm mem bufferSize
      Just b <- allocateBuffer rm mem bufferSize
      Just c <- allocateBuffer rm mem bufferSize

      executeKernel km proc ker [(a,reg),(b,reg)] [(c,reg)] [IntParam $ fromIntegral w, IntParam $ fromIntegral h, BufferParam a, BufferParam b, BufferParam c]

      void $ releaseBuffer rm a
      void $ releaseBuffer rm b
      void $ releaseBuffer rm c

   putStrLn "Done."
