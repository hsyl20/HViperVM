{-# LANGUAGE TupleSections #-}
module ViperVM.Parsing.Lisp where

import ViperVM.Reducer.Graph (newNodeIO, Node)
import qualified ViperVM.Reducer.Graph as G

import Control.Monad (void, forM, foldM)
import Control.Applicative ( (<$>), (<*>) , (<*), (*>))
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
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = optional spaces *> (List <$> sepBy parseExpr spaces)

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
               x <- parseList
               void $ char ')'
               return x

parseModule :: Parser [LispVal]
parseModule = many (parseExpr <* skipMany space) <* eof

readExpr :: String -> IO Node
readExpr input = case parse parseExpr "lisp" input of
   Left err -> error ("Error while parsing Lisp expression: " ++ show err)
   Right a -> makeExpr Map.empty a

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

makeLambda (List [Atom "defun", Atom name, List args, body]) = do
   argNodes <- forM [0.. (length args)-1] (newNodeIO . G.Var)

   let argNames = fmap (\(Atom x) -> x) args
       ctx = Map.fromList $ argNames `zip` (reverse argNodes)
   
   bodyNode <- makeExpr ctx body
   node <- foldM (\n _ -> newNodeIO (G.Abs n)) bodyNode argNodes
   return (name, node)

makeLambda _ = error "Module should only contain function declarations"


-- | Create an expression 
-- Use a context for symbols that must be replaced with nodes
makeExpr :: Map String Node -> LispVal -> IO Node

makeExpr ctx (Atom s) = case Map.lookup s ctx of
                           Just n -> return n
                           Nothing -> newNodeIO (G.Symbol s)

makeExpr ctx (List [Atom "let",bdgs,body]) = do
   let (List assocs2) = bdgs
   bindings <- forM assocs2 (\(List [Atom name, e]) -> (name,) <$> makeExpr ctx e)
   let ctx2 = Map.union ctx (Map.fromList bindings)
   makeExpr ctx2 body

makeExpr ctx (List (x:xs)) = 
   newNodeIO =<< (G.App <$> makeExpr ctx x <*> forM xs (makeExpr ctx))

makeExpr _ (List []) = error "Empty lists are not supported"
makeExpr _ (String _) = error "Strings are not supported"
makeExpr _ (Number i) = newNodeIO (G.ConstInteger i)
makeExpr _ (Bool b) = newNodeIO (G.ConstBool b)
makeExpr ctx (Quote (List x)) = newNodeIO =<< (G.List <$> forM x (makeExpr ctx))
makeExpr _ (Quote a) = error ("Thou shalt not quote " ++ show a)
