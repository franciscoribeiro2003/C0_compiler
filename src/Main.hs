module Main where
import Parser
import Lexer

main :: IO ()
main = do
  passed <- getContents
  print (parser $ alexScanTokens passed)


-- module Main where

-- import Lexer

-- main :: IO ()
-- main = do
--   txt <- getLine
--   print (alexScanTokens txt)
