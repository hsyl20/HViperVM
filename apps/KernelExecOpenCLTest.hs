import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
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
   rm <- createRegionLockManager bm
   km <- createKernelManager rm

   let (w,h) = (1024, 512)
       padding = 11
       primSize = Prim.sizeOf Prim.Float
       bufferSize = (w + padding) * h * primSize
       openclProcs = filter isOpenCLProcessor (processors platform)
       reg = Region2D 0 h (w*primSize) padding

   putStrLn "Registering kernel..." 
   ker <- floatMatrixAddKernelCL

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

      let roRegions = [(a,reg),(b,reg)] 
          rwRegions = [(c,reg)] 
          params = [WordParam $ fromIntegral w, WordParam $ fromIntegral h, BufferParam a, BufferParam b, BufferParam c]

      executeKernel km proc ker roRegions rwRegions params

      void $ releaseBuffer rm a
      void $ releaseBuffer rm b
      void $ releaseBuffer rm c

   putStrLn "Done."
