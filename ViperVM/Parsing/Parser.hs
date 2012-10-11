module ViperVM.Parsing.Parser where

import Language.Haskell.Meta.Parse (parseHsModule)
import Language.Haskell.Exts.Syntax

parse :: String -> Either String Module
parse s = parseHsModule s
