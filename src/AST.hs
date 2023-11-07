module AST where

type Identifier = String

{- 
data Function = Function Type Identifier Param Statement
            | Program [Function]
  deriving (Show, Eq)

data Param = Params [Param]
          | Param Type Identifier
  deriving (Show, Eq)
 -}
 
data Statement = Statment Assignment
               | Statments [Statement]
               | If Expression Statement
               | If_Else Expression Statement Statement
               | While Expression Statement
               | Return Expression
               | For AssignmentFor Expression AssignmentFor Statement
            deriving (Eq, Show)


data AssignmentFor = EmptyFor
  | InitFor Type Identifier Expression
  | SetValueFor Expression Identifier
  | IncrementFor Identifier
  | DecrementFor Identifier
  deriving (Show, Eq)


data Assignment = Assignments [Assignment]
  | Assign Expression
  | Init Type Identifier
  | SetValue Expression Identifier
  | Declaration Expression Identifier
  | Increment Identifier
  | Decrement Identifier
  | PrintInt Expression
  deriving (Show, Eq)


data Expression
  = Var Identifier
  | Num Int
  | Binop Op Expression Expression
  | Op Expression
  | Not Expression
  | Verdadeiro
  | Falso
  | ScanInt
  | ScanStr
  deriving (Show, Eq)


data Op = Plus 
      | Minus 
      | Mult 
      | Div 
      | Mod 
      | Equal 
      | Nequal 
      | Lowert 
      | Lowereq 
      | Greatert
      | Greatereq 
      | And 
      | Or
  deriving (Show, Eq)


data Type = IntType 
      | BoolType 
      | StringType
  deriving (Show, Eq)