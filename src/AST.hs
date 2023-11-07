module AST where

type Identifier = String

{- data Assignment = Assignment Identifier Expression
  deriving (Show, Eq) -}


 
data Statement
    = Assigning Identifier Expression
  -- | IfStatement Expression [Statement] (Maybe [Statement])
  deriving (Show, Eq)
  

data Expression
  = Var Identifier
  | Num Int
  | Binop Op Expression Expression
  -- | UnaryExpression UnaryOperator Expression
  deriving (Show, Eq)
--   | StringLiteral String
--   | BoolLiteral Bool
--   | IntLiteral Int
--   | BinaryExpression BinaryOperator Expression Expression

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


-- data UnaryOperator = Not
--       | Increment
--       | Decrement             
--   deriving (Show, Eq)

data Type = IntType 
      | BoolType 
      | StringType
  deriving (Show, Eq)

data Assignment = Assign Expression
      | Init Type String
      | SetValue Expression String
      | Declaration Expression String
      -- | UnOp UnaryOperator Expression
    deriving (Show, Eq)
  
  {- 
  | While Expression [Statement]
  | For Identifier Expression Expression [Statement]
  | FunctionCall Identifier [Expression]
  | Return (Maybe Expression)

data FunctionDef = FunctionDef Type Identifier [(Type, Identifier)] [Statement]
  deriving (Show, Eq)

data Program = Program [FunctionDef]
  deriving (Show, Eq)
-}

{-

data Program = Program [Statement] deriving (Eq, Show)

data Statement
  = AssignStatement T_Ident Expression
  | IfStatement Expression [Statement] [Statement]
  | WhileStatement Expression [Statement]
  | ForStatement T_Ident Expression Expression [Statement]
  | ReturnStatement Expression
  | PrintIntStatement Expression
  | PrintStringStatement Expression
  deriving (Eq, Show)

data Expression
  = IntLiteral T_Num
  | StringLiteral T_String
  | BoolLiteral T_Bool
  | Identifier T_Ident
  | BinaryExpression T_Plus Expression Expression
  | UnaryExpression T_Not Expression
  deriving (Eq, Show)

data BinaryOperator
  = Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | And
  | Or
  deriving (Eq, Show)

data UnaryOperator
  = Not
  deriving (Eq, Show)
 -}
