import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.ObjectManager
import ViperVM.Platform.BufferManager (createBufferManager)
import ViperVM.Platform.RegionTransferManager

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
   tm <- createRegionTransferManager rm
   om <- createObjectManager tm km

   let (w,h) = (1024, 512)
       padding = 11
       openclProcs = filter isOpenCLProcessor (processors platform)

   putStrLn "Registering kernel..." 
   ker <- floatMatrixAddObjectKernelCL

   putStrLn "OpenCL processors:"
   forM_ openclProcs (putStrLn . show)

   putStrLn "\nCompiling kernel..." 
   validProcs <- compileObjectKernel km ker openclProcs

   putStrLn "Compilation succeeded for processors:" 
   forM_ validProcs (putStrLn . show)
   putStrLn ""

   forM_ validProcs $ \proc -> do

      putStrLn ("Executing kernel on " ++ show proc)
      let mem = head (processorMemories proc)

      Just a <- allocateMatrixObject om mem Prim.Float w h padding
      Just b <- allocateMatrixObject om mem Prim.Float w h padding
      Just c <- allocateMatrixObject om mem Prim.Float w h padding

      executeObjectKernel om proc ker [a,b,c]

      forM_ [a,b,c] (releaseObject om)

   putStrLn "Done."
