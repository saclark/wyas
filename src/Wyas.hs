module Wyas where

import Wyas.Parser
import System.Environment

-- | main
main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))
