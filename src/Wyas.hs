module Wyas where

import Wyas.Eval
import Wyas.Parser
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
