import ViperVM.Platform
import ViperVM.Platform.KernelManager
import ViperVM.Platform.RegionLockManager
import ViperVM.Platform.ObjectManager
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
   om <- createObjectManager rm

   let (w,h) = (1024, 512)
       padding = 11
       openclProcs = filter isOpenCLProcessor (processors platform)
       ker = floatMatrixAddCL

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

      Just a <- allocateMatrixObject om mem Prim.Float w h padding
      Just b <- allocateMatrixObject om mem Prim.Float w h padding
      Just c <- allocateMatrixObject om mem Prim.Float w h padding

      let (params, roRegions, rwRegions) = paramsFromObjects [a,b,c]

      executeKernel km proc ker roRegions rwRegions params

      forM_ [a,b,c] (releaseObject om)

   putStrLn "Done."
