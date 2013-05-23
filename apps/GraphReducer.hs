import ViperVM.Reducer.Graph


main :: IO ()
main = do
   a <- newNodeIO (Data 0)
   b <- newNodeIO (Data 1)
   c <- newNodeIO (Data 2)
   plus <- newNodeIO (Symbol "+")
   mul <- newNodeIO (Symbol "*")
   apb <- newNodeIO (App plus [a,b])
   apb' <- newNodeIO (App plus [a,b])
   apc <- newNodeIO (App plus [a,c])
   apbmapc <- newNodeIO (App mul [apb,apc])
   apbmapcmapb <- newNodeIO (App mul [apbmapc,apb'])

   e <- reduceNode (cse apbmapcmapb)

   case e of
      Data x -> putStrLn $ "Data " ++ show x
      _ -> putStrLn "Reduced to something else than a data"
