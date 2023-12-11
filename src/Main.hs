module Main where
import Parser
import Lexer
import CodeGen1
import CodeGen2

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  passed <- getContents
  print (parser $ alexScanTokens passed)
  let func = parser (alexScanTokens passed)
  let code = evalState (transAst Map.empty func) (0,0)
  print code
  printCode code
