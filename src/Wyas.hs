module Wyas where

import           System.Environment
import           Wyas.Eval
import           Wyas.Parser
import           Wyas.Types         (extractValue, trapError)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
