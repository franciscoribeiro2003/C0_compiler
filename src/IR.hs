module IR where

import AST

type Temp = String
type Label = String


data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOperator Temp Temp Temp
           | OPI BinOperator Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp CompOperator Temp Label Label
           | RETURN Temp
           | PRINTINT Temp
           | PRINTSTR Temp
           | CALL String [Temp]
           | SCANINT
           deriving Show


data FuncIR = FUNCIR String [Temp] [Instr]
              deriving Show
