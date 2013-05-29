{-# LANGUAGE TupleSections #-}
module ViperVM.Parsing.Lisp (
   readModule, readExpr
) where

import ViperVM.Reducer.Graph (newNodeIO, Node)
import qualified ViperVM.Reducer.Graph as G

import Control.Monad (void, forM, foldM)
import Control.Applicative ( (<$>), (<*))
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Map as Map

data LispVal = Atom String
              | List [LispVal]
              | Number Integer
              | String String
              | Bool Bool
              | Quote LispVal
              deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

spaces :: Parser ()
spaces = skipMany1 (space <|> newline)

parseString :: Parser LispVal
parseString = do void $ char '"'
                 x <- many (noneOf "\"")
                 void $ char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol <|> char '\'')
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepEndBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do
   void $ char '\''
   x <- parseExpr
   return $ Quote x


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do void $ char '('
               void $ optional spaces
               x <- parseList
               void $ optional spaces
               void $ char ')'
               return x

parseModule :: Parser [LispVal]
parseModule = many (parseExpr <* skipMany space) <* eof

-- | Parse a Lisp expression
readExpr :: String -> IO Node
readExpr input = case parse parseExpr "lisp" input of
   Left err -> error ("Error while parsing Lisp expression: " ++ show err)
   Right a -> makeExpr Map.empty a

-- | Parse a Lisp module
readModule :: String -> IO (Map String Node)
readModule input = do
   case parse parseModule "lisp" input of
      Left err -> error ("Error while parsing Lisp module: " ++ show err)
      Right a -> Map.fromList <$> forM a makeLambda


-- | Create an abstraction node from a "defun"
makeLambda :: LispVal -> IO (String, Node)

makeLambda (List [Atom "defun", Atom name, List args, String _, List body]) = 
   -- Drop comment string
   makeLambda (List [Atom "defun", Atom name, List args, List body])

makeLambda (List [Atom "defun", Atom name, List args, body]) = 
   (name,) <$> (newNodeIO =<< (G.Lambda args' <$> makeExpr Map.empty body))
   where
      args' = fmap (\(Atom n) -> n) args

makeLambda _ = error "Module should only contain function declarations"


-- | Create an expression 
-- Use a context for symbols that must be replaced with nodes
makeExpr :: Map String Node -> LispVal -> IO Node

makeExpr ctx (Atom s) = case Map.lookup s ctx of
                           Just n -> return n
                           Nothing -> newNodeIO (G.Symbol s)

makeExpr ctx (List [Atom "let",bdgs,body]) = do
   let (List bindings) = bdgs
   bindings' <- Map.fromList <$> forM bindings (\(List [Atom name, e]) -> (name,) <$> makeExpr ctx e)
   body' <- makeExpr ctx body
   newNodeIO (G.Let False bindings' body')

makeExpr ctx (List [Atom "let*",bdgs,body]) = do
   let (List bindings) = bdgs
   bindings' <- Map.fromList <$> forM bindings (\(List [Atom name, e]) -> (name,) <$> makeExpr ctx e)
   body' <- makeExpr ctx body
   newNodeIO (G.Let True bindings' body')

makeExpr ctx (List [x]) = makeExpr ctx x

makeExpr ctx (List (x:xs)) = do
   int <- makeExpr ctx x
   foldM (\a1 a2 -> newNodeIO =<< (G.App a1 <$> makeExpr ctx a2)) int xs

makeExpr _ (List []) = error "Empty lists are not supported"
makeExpr _ (String _) = error "Strings are not supported"
makeExpr _ (Number i) = newNodeIO (G.ConstInteger i)
makeExpr _ (Bool b) = newNodeIO (G.ConstBool b)
makeExpr ctx (Quote (List x)) = newNodeIO =<< (G.List <$> forM x (makeExpr ctx))
makeExpr _ (Quote a) = error ("Thou shalt not quote " ++ show a)
