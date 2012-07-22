module ViperVM.Logging.Logger where

import ViperVM.Platform
import ViperVM.Kernel
import ViperVM.Event


import Text.Printf ( printf )
import System.CPUTime

type TimedAction = (Integer,Integer)

start :: TimedAction -> Integer
start = fst

end :: TimedAction -> Integer
end = snd

duration :: TimedAction -> Integer
duration x = end x - start x

durationSecs :: TimedAction -> String
durationSecs x = printf "%0.3f sec" diff
  where
    diff :: Double
    diff = fromInteger (duration x) / 1e12

timeAction :: IO a -> IO (a,TimedAction)
timeAction f = do
  s <- getCPUTime
  r <- f
  e <- getCPUTime
  return (r, (s,e))

data LogMessage = StopLogger (Event ()) |
                  Custom String |
                  KernelCompilation Kernel [Processor] [Maybe CompiledKernel] TimedAction

type Logger = LogMessage -> IO ()
