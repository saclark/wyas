module Wyas.Types (LispVal(..)) where

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | List [LispVal]
             | Number Integer
             | String String

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom val)          = val
showVal (Bool True)         = "#t"
showVal (Bool False)        = "#f"
showVal (Character val)     = show val
showVal (Number val)        = show val
showVal (DottedList xs val) = concat ["(", unwordsShowVal xs, " . ", showVal val, ")"]
showVal (List xs)           = concat ["(", unwordsShowVal xs, ")"]
showVal (String val)        = concat ["\"", val, "\""]

unwordsShowVal :: [LispVal] -> String
unwordsShowVal = unwords . map showVal
