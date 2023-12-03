module AST where

type Declaration = (Type, String)


data Type = IntType
          | BoolType
          | StringType
  deriving Show


data Function = Function Type String [Declaration] [Statement]
  deriving Show


data Operation = AssignOp String Exp
        | PosIncrement String
        | PosDecrement String
        | PreIncrement String
        | PreDecrement String
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
              | AssignScanInt String
              | Init Type [Exp]
              | Declaration Type String Exp
  deriving Show


data ForOperation = EmptyFor
                | ForAssign String Exp
                | ForDeclaration Type String Exp
  deriving Show


data Statement = VarOp StatementOp
        | IfElse CompareExpression Statement Statement
        | If CompareExpression Statement
        | While CompareExpression Statement
        | For ForOperation CompareExpression Operation Statement
        | StatementsBlock [Statement]
        | FunctionCallStm String [Exp]
        | PrintInt Exp
        | PrintStr Exp
        | Return [Exp]
  deriving Show


data Exp = Num Int
         | Str String
         | Var String
         | ScanIntExp
         | FunctionCallExp String [Exp]
         | Op BinOperator Exp Exp
         | BooleanConst Bool
  deriving Show


data CompareExpression = Comp CompOperator Exp Exp
                    | And CompareExpression CompareExpression
                    | Or CompareExpression CompareExpression
                    | Not CompareExpression
                    | FunctionCallComp String [Exp]
  deriving Show
