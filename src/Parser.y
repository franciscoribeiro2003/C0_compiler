{
module Parser where
import Lexer
import AST
}

%name parser
%tokentype { Token }
%error     { parseError }

%token

-- palavras reservadas
true     { TRUE_TOKEN $$ }
false    { FALSE_TOKEN $$ }
return   { RETURN_TOKEN }
num      { NUM_TOKEN $$ }
str      { STRING_TOKEN $$ }
id       { VAR_TOKEN $$ }

-- tipos
int  { INT_DEF_TOKEN }
bool { BOOL_DEF_TOKEN }
string { STRING_DEF_TOKEN }

-- delimitadores
'=' { ASSIGN_TOKEN }
'{' { LBRACE_TOKEN }
'}' { RBRACE_TOKEN }
'(' { LPAREN_TOKEN }
')' { RPAREN_TOKEN }

-- operadores
"++"{ INCR_TOKEN }
"--"{ DECR_TOKEN }
'*' { MULT_TOKEN }
'/' { DIV_TOKEN }
'%' { MOD_TOKEN }
';' { SEMICOLON_TOKEN }
',' { COLON_TOKEN }
'+' { PLUS_TOKEN }
'-' { MINUS_TOKEN }

-- comparadores
"=="{ EQUAL_TOKEN }
"!="{ NEQUAL_TOKEN }
"<" { LTHEN_TOKEN }
"<="{ LTOE_TOKEN }
">" { GTHEN_TOKEN }
">="{ GTOE_TOKEN }

-- operadores lógicos
"&&" { AND_TOKEN }
"||" { OR_TOKEN }
'!'  { NOT_TOKEN }

-- condicionais
if { IF_TOKEN }
else { ELSE_TOKEN }

--ciclos
while { WHILE_TOKEN }
for { FOR_TOKEN }

-- input/output
-- print_int { PRINTINT_TOKEN }
scan_int  { SCANINT_TOKEN }
-- print_str { PRINTSTR_TOKEN }

-- associatividade e precedência
%left "<" ">" "==" "!=" ">=" "<=" "&&" "||"
%left '+' '-'
%left '*' '/' '%'
%right '!' "++" "--"

%%

-- Gramática

Functions : Function           { [$1] }
          | Functions Function { $1 ++ [$2] }


Function : Type id '(' Declaration ')' '{' Statements '}' { Function $1 $2 $4 $7 }


Statement : StatementOp                                                         { VarOp $1 }
          | if '(' CompareExpression ')' Statement else Statement               { IfElse $3 $5 $7 }
          | if '(' CompareExpression ')' Statement                              { If $3 $5 }
          | while '(' CompareExpression ')' Statement                           { While $3 $5 }
          | for '(' ForOperation CompareExpression ';' Operation ')' Statement  { For $3 $4 $6 $8 }
          | '{' Statements '}'                                                  { StatementsBlock $2 }
          | id '(' Expressions ')' ';'                                          { FunctionCallStm $1 $3 }
          | return Expressions ';'                                              { Return $2 }
          -- | print_int '(' Expression ')' ';'                                    { PrintInt $3 }
          -- | print_str '(' Expression ')' ';'                                    { PrintStr $3 }


Expression : num                        { Num $1}
          | str                         { Str $1}
          | id                          { Var $1 }
          | '(' Expression ')'          { $2 }
          | id '(' Expressions ')'      { FunctionCallExp $1 $3 }
          | Expression '+' Expression   { Op Add $1 $3 }
          | Expression '-' Expression   { Op Minus $1 $3 }
          | Expression '*' Expression   { Op Mult $1 $3 }
          | Expression '/' Expression   { Op Div $1 $3 }
          | Expression '%' Expression   { Op Mod $1 $3 }
          | true                        { BooleanConst True }
          | false                       { BooleanConst False }
          --| scan_int '(' ')'            { ScanIntExp }


Operation : id '=' Expression { AssignOp $1 $3 }
          | id "++"           { PosIncrement $1 }
          | id "--"           { PosDecrement $1 }
          | "++" id           { PreIncrement $2 }
          | "--" id           { PreDecrement $2 }


StatementOp : id '=' Expression ';'     { AssignStm $1 $3 }
          | Type Expressions ';'        { Init $1 $2 }
          | Type id '=' Expression ';'  { Declaration $1 $2 $4 }
          -- | id '=' scan_int '(' ')' ';' { AssignScanInt $1 }


ForOperation : ';'                      { EmptyFor }
          | id '=' Expression ';'       { ForAssign $1 $3 }
          | Type id '=' Expression ';'  { ForDeclaration $1 $2 $4 }


CompareExpression : Expression "==" Expression                   { Comp Equal $1 $3 }
                  | Expression "!=" Expression                   { Comp Nequal $1 $3 }
                  | Expression "<" Expression                    { Comp Lowert $1 $3 }
                  | Expression "<=" Expression                   { Comp Lowereq $1 $3 }
                  | Expression ">" Expression                    { Comp Greatert $1 $3 }
                  | Expression ">=" Expression                   { Comp Greatereq $1 $3 }
                  | CompareExpression "&&" CompareExpression     { And $1 $3 }
                  | CompareExpression "||" CompareExpression     { Or $1 $3 }
                  | '!' CompareExpression                        { Not $2 }
                  | id '(' Expressions ')'                       { FunctionCallComp $1 $3 }


Statements : {- empty -}             { [] }
          | Statements Statement     { $1 ++ [$2] }


Declaration : {- empty -}               { [] }
          | Type id                     { [($1, $2)] }
          | Declaration ',' Type id     { $1 ++ [($3, $4)] }


Expressions : {- empty -}              { [] }
          | Expression                 { [$1] }
          | Expressions ',' Expression { $1 ++ [$3] }


Type : int     { IntType }
     | bool    { BoolType }
     | string  { StringType }


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}
