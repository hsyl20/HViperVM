import ViperVM.Reducer.Graph

import Data.Map as Map


main :: IO ()
main = do
   a <- newNodeIO (Data 0)
   b <- newNodeIO (Data 1)
   c <- newNodeIO (Data 2)
   d <- newNodeIO (Data 3)
   plus <- newNodeIO (Symbol "+")
   mul <- newNodeIO (Symbol "*")
   apb <- newNodeIO (App plus [a,b])
   apb' <- newNodeIO (App plus [a,b]) -- For CSE test
   apc <- newNodeIO (App plus [a,c])
   apbmapc <- newNodeIO (App mul [apb,apc])
   apbmapcmapb <- newNodeIO (App mul [apbmapc,apb'])

   e <- reduceNodeExpr Map.empty (cse apbmapcmapb)
   case e of
      Data x -> putStrLn $ "Data " ++ show x
      _ -> putStrLn "Reduced to something else than a data"

   v0 <- newNodeIO (Var 0)
   v1 <- newNodeIO (Var 1)
   pv0 <- newNodeIO (App plus [v0])
   pv0v1 <- newNodeIO (App pv0 [v1])
   apv0v1 <- newNodeIO (Abs pv0v1)
   add <- newNodeIO (Abs apv0v1)
   addab <- newNodeIO (App add [a,b])
   addcd <- newNodeIO (App add [c,d])
   addabcd <- newNodeIO (App add [addab,addcd])

   putStrLn (show addabcd)

   f <- reduceNodeExpr Map.empty (addabcd)
   case f of
      Data x -> putStrLn $ "Data " ++ show x
      res -> putStrLn ("Reduced to something else than a data: " ++ show res)


