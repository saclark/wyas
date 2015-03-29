module Wyas.Parser where

import Wyas.Types
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapableChar :: Parser Char
escapableChar = oneOf "rnt\\"

escapedChar :: Parser Char
escapedChar = char '\\' >> escapableChar

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
-- parseString = char '"' >> many (noneOf "\"" <|> escapedChar) >>= \x -> char '"' >> return (String x)
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"" <|> escapedChar)
                _ <- char '"'
                return $ String x

parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= return . Number . read
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _ -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseNumber <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "Syntax error in " ++ show err
                   Right val -> "Valid: " ++ show val
