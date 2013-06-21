module ViperVM.Backends.Host.Memory (
   Memory, memorySize, memoryName,
   initMemory
) where

import Data.Word
import Text.Printf
import Text.ParserCombinators.Parsec
import qualified System.Info
import Control.Applicative ( (<$>))

data Memory = Memory {
   memorySize :: Word64
} deriving (Eq, Ord)


instance Show Memory where
   show = memoryName

-- | Initialize host memory
initMemory :: IO Memory
initMemory = do
   size <- retrieveMemorySize
   return (Memory size)

memoryName :: Memory -> String
memoryName _ = "Host Memory"

-- | Retrieve the host memory size
retrieveMemorySize :: IO Word64
retrieveMemorySize = do

   case System.Info.os of
      "linux" -> retrieveMemorySizeLinux
      os -> error (printf "Cannot retrieve memory size on \"%s\" operating system" os)

-- | Retrieve host memory size on Linux
-- Parse /proc/meminfo
retrieveMemorySizeLinux :: IO Word64
retrieveMemorySizeLinux = do
   s <- readFile "/proc/meminfo"

   let skipManyTill p end = end <|> (p >> skipManyTill p end)

       parser = skipManyTill (many anyChar >> newline) $ do
                    _ <- string "MemTotal:" >> spaces
                    v <- read <$> many1 digit
                    _ <- spaces >> string "kB" >> newline
                    return v
   
   case parse parser "/proc/meminfo" s of
      Left err -> error ("Error while parsing /proc/meminfo" ++ show err)
      Right a -> return (a * 1024)
