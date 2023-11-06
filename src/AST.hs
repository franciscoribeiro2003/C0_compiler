module AST where

data Type = Int
          | Bool
          | String
          deriving Show

-- Definindo o tipo da AST
data Exp = Add Exp Exp
         | Subtraction Exp Exp
         | Multiplication Exp Exp
         | Division Exp Exp
         | Module Exp Exp
         | Equal Exp Exp
         | Compare Exp Exp
         | NotEquals Exp Exp
         | Less Exp Exp
         | LessEquals Exp Exp
         | Greater Exp Exp
         | GreaterEquals Exp Exp
         | And Exp Exp
         | Or Exp Exp
         deriving Show

-- data parseError = ParseError String
--                 deriving Show
