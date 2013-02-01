module ViperVM.Parsing.Parser where

import Language.Haskell.Meta.Parse (parseHsModule)
import Language.Haskell.Exts.Syntax

import Data.Maybe (listToMaybe)

parse :: String -> Either String Module
parse = parseHsModule


moduleDecl :: Module -> [Decl]
moduleDecl (Module _ _ _ _ _ _ dcls) = dcls

declIdent :: Decl -> Name
declIdent d = case d of
  TypeDecl _ n _ _ -> n
  TypeSig _ n _ -> head n
  _ -> Ident ""

execute :: Module -> String -> IO ()
execute m fn = do
  let decls = moduleDecl m
  let typ = listToMaybe $ filter (\x -> declIdent x == Ident fn) decls

  print typ

  return ()
