import ViperVM.Reducer.Graph

import Data.Map as Map
import Control.Monad (foldM)
import Control.Applicative ( (<$>) )


main :: IO ()
main = do
   a <- newNodeIO (Data 0)
   b <- newNodeIO (Data 1)
   c <- newNodeIO (Data 2)
   d <- newNodeIO (Data 3)
   plus <- newNodeIO (Symbol "+")
   mul <- newNodeIO (Symbol "*")
   apb <- appN plus [a,b]
   apb' <- appN plus [a,b] -- For CSE test
   apc <- appN plus [a,c]
   apbmapc <- appN mul [apb,apc]
   apbmapcmapb <- appN mul [apbmapc,apb']

   check Map.empty apbmapcmapb

   x <- newNodeIO (Symbol "x")
   y <- newNodeIO (Symbol "y")
   add <- newNodeIO =<< (Lambda ["x","y"] <$> appN plus [x,y])
   addab <- appN add [a,b]
   addcd <- appN add [c,d]
   addabcd <- appN add [addab,addcd]

   putStrLn (show addabcd)

   check Map.empty addabcd


check :: Map String Node -> Node -> IO ()
check ctx r = do
      putStrLn ("Evaluating: " ++ show r)
      f <- run ctx r
      putStrLn ("Reduction result: " ++ show f)

app :: Node -> Node -> IO Node
app x y = newNodeIO (App x y)

appN :: Node -> [Node] -> IO Node
appN x ys = foldM app x ys
