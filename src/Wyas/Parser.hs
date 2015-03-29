module Wyas.Parser
    ( parseNumber
    , parseCharLiteral
    , parseString
    , parseAtom
    , parseList
    , parseQuotedList
    , parseDottedList
    , parseExpr
    , readExpr
    ) where

import Wyas.Types
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escaped :: Parser Char
escaped = char '\\' >> choice specialCharParsers
  where
    specialCharParsers           = zipWith escapedChar specialCharCodes specialChars
    escapedChar code specialChar = char code >> return specialChar
    specialCharCodes             = ['f',  'n',  'r',  't',  '\\', '"']
    specialChars                 = ['\f', '\n', '\r', '\t', '\\', '\"']

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseCharLiteral :: Parser LispVal
parseCharLiteral = liftM Character (char '#' >> char '\\' >> anyChar)

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (escaped <|> noneOf "\"")
                _ <- char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _ -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseQuotedList :: Parser LispVal
parseQuotedList = do
                    _ <- char '\''
                    x <- parseExpr
                    return $ List [Atom "quote", x]

parseDottedList :: Parser LispVal
parseDottedList = do
                    listHead <- endBy parseExpr spaces
                    listTail <- char '.' >> spaces >> parseExpr
                    return $ DottedList listHead listTail

parseExpr :: Parser LispVal
parseExpr =  parseNumber
         <|> parseCharLiteral
         <|> parseString
         <|> parseAtom
         <|> parseQuotedList
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "Error: " ++ show err
                   Right val -> val
