module Wyas.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input =
  case result of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
  where
    result = parse (spaces >> symbol) "list" input
