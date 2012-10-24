import ViperVM.Parsing.Parser

import Paths_ViperVM

main :: IO ()
main = do
   input <- getDataFileName "data/Codelets/Blas/Lu.vvm"
   file <- readFile input
   putStrLn $ show $ parse file
