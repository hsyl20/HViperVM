import ViperVM.Parsing.Parser
import Control.Applicative

import Paths_ViperVM

main :: IO ()
main = do
   let f = "data/Codelets/Blas/Lu.vvm"
   
   p <- parse <$> (readFile =<< getDataFileName f)
    
   putStrLn ("Parsing " ++ f)

   putStrLn (show p)

   putStrLn "Searching for function \"dummy\""

   case p of
      Right m -> execute m "dummy"
      _ -> return ()

