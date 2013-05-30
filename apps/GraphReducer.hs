import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import Data.Map as Map
import Control.Monad (foldM)
import Control.Applicative ( (<$>) )


main :: IO ()
main = do
   a <- newNodeIO (ConstInteger 0)
   b <- newNodeIO (ConstInteger 1)
   c <- newNodeIO (ConstInteger 2)
   d <- newNodeIO (ConstInteger 3)
   plus <- newNodeIO (Symbol "+")
   mul <- newNodeIO (Symbol "*")
   apb <- appN plus [a,b]
   apb' <- appN plus [a,b]
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

   check Map.empty addabcd


check :: Map String Node -> Node -> IO ()
check ctx r = do
      putStrLn ("Evaluating: " ++ show r)
      f <- run defaultBuiltins ctx r
      putStrLn ("Reduction result: " ++ show f)

app :: Node -> Node -> IO Node
app x y = newNodeIO (App x y)

appN :: Node -> [Node] -> IO Node
appN x ys = foldM app x ys
