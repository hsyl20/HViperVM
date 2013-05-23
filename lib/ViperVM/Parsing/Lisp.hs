module ViperVM.Parsing.Lisp where

import ViperVM.Reducer.Graph

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    hd <- endBy parseExpr spaces
    tl <- char '.' >> spaces >> parseExpr
    return $ DottedList hd tl

parseQuoted :: Parser LispVal
parseQuoted = do
   void $ char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do void $ char '('
               x <- try parseList <|> parseDottedList
               void $ char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right a -> "Found value: " ++ show a

--parse :: String -> Map String Node
--parse s =
