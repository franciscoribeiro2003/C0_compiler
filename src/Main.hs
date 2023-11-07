module Main where
import Parser
import Lexer

main :: IO ()
main = do
  txt <- getLine
  print (parser $ alexScanTokens txt)
