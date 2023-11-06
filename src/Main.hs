module Main where

import AST
import Lexer
import Parser

main :: IO ()
main = do
  txt <- getLine
  print (parse $ alexScanTokens txt)
