import ViperVM
import ViperVM.Platform
import ViperVM.Scheduler
import Data.Traversable

main = do
  let cllib = "/usr/lib/libOpenCL.so"

  platform <- initPlatform cllib
  putStrLn =<< platformInfo platform

  putStrLn "Starting the scheduler..."
  sched <- startScheduler platform

  putStrLn "Stopping the scheduler..."
  stopScheduler sched

  putStrLn "Done."
