module ViperVM.Logging.Logger where

import ViperVM.Platform
import ViperVM.Kernel
import Text.Printf ( printf )

type TimedAction = (Integer,Integer)

start :: TimedAction -> Integer
start = fst

end :: TimedAction -> Integer
end = snd

duration :: TimedAction -> Integer
duration x = (end x) - (start x)

durationSecs :: TimedAction -> String
durationSecs x = printf "%0.3f sec" diff
  where
    diff :: Double
    diff = (fromInteger $ duration x) / 1e12

data LogMessage = StopLogger |
                  Custom String |
                  KernelCompilation Kernel [Processor] [Maybe CompiledKernel] TimedAction

type Logger = LogMessage -> IO ()
