import ViperVM.Parsing.Parser

import Paths_ViperVM

main :: IO ()
main = do
   let f = "data/Codelets/Blas/Lu.vvm"
   
   input <- getDataFileName f
   file <- readFile input
   let p = parse file
    
   putStrLn $ "Parsing " ++ f

   putStrLn $ show $ p

   putStrLn $ "Searching for function \"dummy\""

   case p of
     Right m -> execute m "dummy"
     _ -> return ()

