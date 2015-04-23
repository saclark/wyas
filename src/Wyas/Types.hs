module Wyas.Types (LispVal(..)) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error

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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show = showError

instance Error LispError where
  noMsg = Default "An error has ocurred"
  strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varName) = concat [message, ": ", varName]
showError (BadSpecialForm message form) = concat [message, ": ", show form]
showError (NotFunction message func) = concat [message, ": ", func]
showError (NumArgs expected found) =
  concat ["Expected: ", show expected, " args; found values ", unwordsShowVal found]
showError (TypeMismatch expected found) =
  concat ["Invalid type: expected ", expected, "found, ", show found]
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = message

type ThrowsError = Either LispError

-- trapError :: Either LispError String -> Either LispError String
trapError action = catchError action (return . show)
