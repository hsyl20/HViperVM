import ViperVM.Parsing.Parser

main = do
   file <- readFile "./Codelets/Blas/Lu.vvm"
   putStrLn $ show $ parse file
