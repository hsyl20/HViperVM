module ViperVM.Parsing.Lisp where

import ViperVM.Reducer.Graph

import Control.Monad (void, forM, liftM, foldM)
import Control.Applicative ( (<$>), (<*>) )
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Map as Map

data LispVal = Atom String
              | List [LispVal]
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
               x <- parseList
               void $ char ')'
               return x

parseModule :: Parser [LispVal]
parseModule = many parseExpr

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
   argNodes <- forM [0.. (length args)-1] (newNodeIO . Var)

   let argNames = fmap (\(Atom x) -> x) args
       ctx = Map.fromList $ argNames `zip` argNodes
   
   bodyNode <- makeExpr ctx body
   node <- foldM (\n _ -> newNodeIO (Abs n)) bodyNode argNodes
   return (name, node)

makeLambda _ = error "Module should only contain function declarations"


-- | Create an expression 
-- Use a context for symbols that must be replaced with nodes
makeExpr :: Map String Node -> LispVal -> IO Node

makeExpr ctx (Atom s) = case Map.lookup s ctx of
                           Just n -> return n
                           Nothing -> newNodeIO (Symbol s)

makeExpr ctx (List (x:xs)) = 
   newNodeIO =<< (App <$> makeExpr ctx x <*> forM xs (makeExpr ctx))

makeExpr _ (List []) = error "Empty lists are not supported"
makeExpr _ (String _) = error "Strings are not supported"
makeExpr _ (Number i) = newNodeIO (ConstInteger i)
makeExpr _ (Bool b) = newNodeIO (ConstBool b)
