module AST where

type Declaration = (Type, String)


data Type = IntType
          | BoolType
          | StringType
  deriving Show


data Function = Function Type String [Declaration] [Statement]
  deriving Show


data Operation = PreIncrement String
        | PosIncrement String
        | PreDecrement String
        | PosDecrement String
        | AssignOp String Exp
  deriving Show


data BinOperator = Add
              | Minus
              | Mult
              | Div
              | Mod
  deriving Show


data CompOperator = Lowert
              | Greatert
              | Lowereq
              | Greatereq
              | Equal
              | Nequal
  deriving Show


data StatementOp = AssignStm String Exp
              | Init Type String
              | Declaration Type String Exp
              | ScanInt String
  deriving Show


data ForOperation = ForAssign String Exp
                | ForDeclaration Type String Exp
                | EmptyFor
  deriving Show


data Statement = VarOp StatementOp
        | If CompareExpression Statement
        | IfElse CompareExpression Statement Statement
        | While CompareExpression Statement
        | For ForOperation CompareExpression Operation Statement
        | FunctionCallStm String [Exp]
        | PrintInt Exp
        | PrintStr Exp
        | StatementsBlock [Statement]
        | Return Exp
  deriving Show


data Exp = Num Int
         | Str String
         | Var String
         | BooleanConst Bool
         | Op BinOperator Exp Exp
         | FunctionCallExp String [Exp]
  deriving Show


data CompareExpression = Comp CompOperator Exp Exp
                    | And CompareExpression CompareExpression
                    | Or CompareExpression CompareExpression
                    | Not CompareExpression
                    | FunctionCallComp String [Exp]
  deriving Show
