import ViperVM.Graph

import Control.Concurrent.STM
import Data.IntSet


data Task = Task String deriving (Show)

main :: IO ()
main = do
   g <- atomically newGraph :: IO (Graph Int)

   (node1,node2) <- atomically $ do
      n1 <- addNode g 10 empty
      n2 <- addNode g 20 empty
      n3 <- addNode g 30 empty
      addNode_ g 40 (fromList [n1,n2,n3])
      return (n1,n2)

   atomically $ do
      v1 <- nodeValue g node1
      v2 <- nodeValue g node2
      setNodeValue g node1 (v1+v2*100)

   atomically $ do
      removeNode g node2

   putStrLn =<< atomically (printGraph g)



   g2 <- atomically newGraph :: IO (Graph Task)

   atomically $ do
      n1 <- addNode g2 (Task "dtrsm") empty
      n2 <- addNode g2 (Task "dsymm") empty
      n3 <- addNode g2 (Task "dsyrk") empty
      addNode_ g2 (Task "dgemm") (fromList [n1,n2,n3])

   putStrLn =<< atomically (printGraph g2)
