module Main where
import Parser
import Lexer

main :: IO ()
main = do
  txt <- getLine
  print (parser $ alexScanTokens txt)

{-



---
 module Main where

import Lexer

main :: IO ()
main = do
  txt <- getLine
  print (alexScanTokens txt)
 -}