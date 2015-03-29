module Wyas.Eval where

import Wyas.Types

eval :: LispVal -> LispVal
eval val@(Bool _)               = val
eval val@(Number _)             = val
eval val@(Character _)          = val
eval val@(String _)             = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f:xs))         = apply f $ map eval xs
eval _                          = String "Not yet implemented"

apply :: String -> [LispVal] -> LispVal
apply f xs = maybe (Bool False) ($ xs) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+",         numericBinop (+)),
              ("-",         numericBinop (-)),
              ("*",         numericBinop (*)),
              ("/",         numericBinop div),
              ("mod",       numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
