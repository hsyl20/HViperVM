{-# LANGUAGE TupleSections, LambdaCase #-}
import ViperVM.Parsing.Lisp
import ViperVM.Graph.Graph
import ViperVM.Graph.ParallelReducer
import ViperVM.Graph.Builtins

import Control.Applicative ( (<$>) )
import Control.Monad (replicateM)
import Data.Map as Map
import Data.Dynamic

import Paths_ViperVM

main :: IO ()
main = do
   let file = "apps/samples/lisp/Sample.lisp"
   
       kernels = Map.fromList [
         ("potrf", Builtin [True] $ \case
            ([m],[arg]) -> do
               let sz = readData m
               if sz > 100
                  then do
                     splt <- readExpr "(lambda (x) (unsplit (deepSeq (cholRec (triangularize (split 2 2 x))))))"
                     return (App splt arg)
                  else do
                     putStrLn ("potrf (" ++ show sz ++ ")")
                     return m
            _ -> error "Invalid parameters"
         ),

         ("trsm", Builtin [True,True] $ \case
            (args,_) -> do
               let sz = readData (head args)
               putStrLn ("trsm (" ++ show sz ++ ")")
               return (head args)
         ),

         ("syrk", Builtin [True,True] $ \case
            (args,_) -> do
               let sz = readData (head args)
               putStrLn ("syrk (" ++ show sz ++ ")")
               return (head args)
         ),

         ("sgemm", Builtin [True,True,True] $ \case
            (args,_) -> do
               let sz = readData (head args)
               putStrLn ("sgemm (" ++ show sz ++ ")")
               return (head args)

         ),

         ("unsplit", Builtin [True] $ \case
            ([List xs],_) -> do
               List ys <- getNodeExprIO (head xs)
               sz <- readData <$> getNodeExprIO (head ys)
               let sz' = sz * (length xs) :: Int
               putStrLn ("unsplit: " ++ show sz ++ " -> " ++ show sz')
               return (Data (toDyn sz'))
            _ -> error "Invalid parameters"
         ),

         ("split", Builtin [True,True,True] $ \case
            ([ConstInteger h, ConstInteger w, m],_) -> do
               let sz = readData m :: Int
                   h' = fromIntegral h
                   w' = fromIntegral w
                   sz' = sz `div` w'
                   d' = Data (toDyn sz')
               putStrLn ("split: " ++ show sz ++ " -> " ++ show sz')

               List <$> replicateM h' (newNodeIO . List =<< replicateM w' (newNodeIO d'))
            _ -> error "Invalid parameters"

         )

         ] 

   ctx <- readModule =<< readFile =<< getDataFileName file


   let ch  = check builtins ctx
       datas = registerData [("m", 500 :: Int)]
       builtins = Map.unions [defaultBuiltins, kernels, datas]


   ch "(potrf m)"
   --ch "(unsplit (split 2 2 m))"


check :: Map String Builtin -> Map String Node -> String -> IO ()
check builtins ctx expr = do
      r <- readExpr expr

      putStrLn ("Evaluating: " ++ show expr)
      f <- run builtins ctx r
      putStrLn ("Reduction result: " ++ show f)


registerData :: Typeable a => [(String,a)] -> Map String Builtin
registerData ds = fmap f (Map.fromList ds)
   where
      f = Builtin [] . const . return . Data . toDyn


readData :: Expr -> Int
readData (Data u) = fromDyn u (error "Invalid data")
readData e = error ("Invalid data parameter: " ++ show e)
