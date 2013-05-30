{-# LANGUAGE LambdaCase #-}
module ViperVM.Graph.Builtins where

import ViperVM.Graph.Graph
import Data.Map as Map

data Builtin = Builtin {
   evals :: [Bool],
   action :: ([Expr],[Node]) -> IO Expr
}

defaultBuiltins :: Map Name Builtin
defaultBuiltins = Map.unions [
      conditionBuiltins,
      arithmeticBuiltins,
      listBuiltins
   ]


conditionBuiltins :: Map String Builtin
conditionBuiltins = Map.fromList [
   ("if", Builtin [True,False,False] $ \case
      ([ConstBool True],[_,thn,_]) -> return (Alias thn)
      ([ConstBool False],[_,_,els]) -> return (Alias els)
      (e,_) -> error ("If condition does not evaluate to a boolean: " ++ show e))
   ]

arithmeticBuiltins :: Map String Builtin
arithmeticBuiltins = Map.fromList [

   ("+", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y], _) -> return (ConstInteger (x+y))
      (e,_) -> error ("Do not know how to add this: " ++ show e)),

   ("-", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstInteger (x-y))
      (e,_) -> error ("Do not know how to subtract this: " ++ show e)),

   ("*", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstInteger (x*y))
      (e,_) -> error ("Do not know how to multiply this: " ++ show e)),

   ("==", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x == y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e)),

   ("/=", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x /= y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e)),

   (">", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x > y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e)),

   ("<", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x < y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e)),

   (">=", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x >= y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e)),

   ("<=", Builtin [True,True] $ \case
      ([ConstInteger x, ConstInteger y],_) -> return (ConstBool (x <= y))
      (e,_) -> error ("Do not know how to compare this: " ++ show e))

   ]

listBuiltins :: Map String Builtin
listBuiltins = Map.fromList [

   ("List.head", Builtin [True] $ \case
      ([List []],_) -> error ("List.head applied to an empty list")
      ([List (x:_)],_) -> return (Alias x)
      (e,_) -> error ("List.head can only be applied to a list (found " ++ show e ++")")),

   ("List.tail", Builtin [True] $ \case
      ([List (_:xs)],_) -> return (List xs)
      (e,_) -> error ("List.tail can only be applied to a list (found " ++ show e ++")")),

   ("List.drop", Builtin [True,True] $ \case
      ([ConstInteger n, List xs],_) -> return (List (drop (fromIntegral n) xs))
      (e,_) -> error ("List.drop cannot be applied (found " ++ show e ++")")),

   ("List.null", Builtin [True] $ \case
      ([List xs],_) -> return (ConstBool (Prelude.null xs))
      (e,_) -> error ("List.null can only be applied to a list (found " ++ show e ++")")),

   ("List.cons", Builtin [False,True] $ \case
      ([List xs],[x,_]) -> return (List (x:xs))
      (e,_) -> error ("List.cons can only be applied to a list (found " ++ show e ++")")),

   ("List.snoc", Builtin [True,False] $ \case
      ([List xs],[_,x]) -> return (List (xs ++ [x]))
      (e,_) -> error ("List.snoc cannot be applied (found " ++ show e ++")"))

   ]

