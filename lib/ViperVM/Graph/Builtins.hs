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
      (e,_) -> error ("If condition does not evaluate to a boolean: " ++ show e)),

   ("or", Builtin [True,False] $ \case
      ([ConstBool True],_) -> return (ConstBool True)
      ([ConstBool False],[_,els]) -> return (Alias els)
      (e,_) -> error ("Or parameter does not evaluate to a boolean: " ++ show e)),

   ("and", Builtin [True,True] $ \case
      ([ConstBool a, ConstBool b],_) -> return (ConstBool (a && b))
      (e,_) -> error ("And parameters do not evaluate to booleans: " ++ show e))
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
      ([ListNil],_) -> error ("List.head applied to an empty list")
      ([ListCons x _ ],_) -> return (Alias x)
      (e,_) -> error ("List.head can only be applied to a list (found " ++ show e ++")")),

   ("List.tail", Builtin [True] $ \case
      ([ListCons _ xs],_) -> return (Alias xs)
      (e,_) -> error ("List.tail can only be applied to a list (found " ++ show e ++")")),

   ("List.null", Builtin [True] $ \case
      ([ListNil],_) -> return (ConstBool True)
      ([ListCons _ _],_) -> return (ConstBool False)
      (e,_) -> error ("List.null can only be applied to a list (found " ++ show e ++")")),

   ("List.cons", Builtin [False,False] $ \case
      (_,[x,xs]) -> return (ListCons x xs)
      (_,_) -> error ("List.cons can only be applied to a list"))

   ]

