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
num      { NUM_TOK $$ }
str      { STRING_TOK $$ }
id       { VAR_TOK $$ }
true     { TRUE_TOK $$ }
false    { FALSE_TOK $$ }
return   { RETURN_TOK }

-- tipos
int  { INT_DEF_TOK }
bool { BOOL_DEF_TOK }
string { STRING_DEF_TOK }

-- delimitadores
'(' { LPAREN_TOK }
')' { RPAREN_TOK }
'=' { ASSIGN_TOK }
'{' { LBRACE_TOK }
'}' { RBRACE_TOK }

-- operadores
'+' { PLUS_TOK }
'-' { MINUS_TOK }
'*' { MULT_TOK }
'/' { DIV_TOK }
'%' { MOD_TOK }
';' { SEMICOLON_TOK }
',' { COLON_TOK }
"++"{ INCR_TOK }
"--"{ DECR_TOK }

-- comparadores
"=="{ EQUAL_TOK }
"!="{ NEQUAL_TOK }
"<" { LTHEN_TOK }
">" { GTHEN_TOK }
">="{ GTOE_TOK }
"<="{ LTOE_TOK }

-- operadores lógicos
"&&" { AND_TOK }
"||" { OR_TOK }
'!'  { NOT_TOK }

-- condicionais
if { IF_TOK }
else { ELSE_TOK }

--ciclos
while { WHILE_TOK }
for { FOR_TOK }

-- input/output
print_int { PRINTINT_TOK }
scan_int  { SCANINT_TOK }
print_str { PRINTSTR_TOK }

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
          | print_int '(' Expression ')' ';'                                    { PrintInt $3 }
          | print_str '(' Expression ')' ';'                                    { PrintStr $3 }
          | return Expressions ';'                                              { Return $2 }


Expression : num                        { Num $1}
          | str                         { Str $1}
          | id                          { Var $1 }
          | scan_int '(' ')'            { ScanIntExp }
          | '(' Expression ')'          { $2 }
          | id '(' Expressions ')'      { FunctionCallExp $1 $3 }
          | Expression '+' Expression   { Op Add $1 $3 }
          | Expression '-' Expression   { Op Minus $1 $3 }
          | Expression '*' Expression   { Op Mult $1 $3 }
          | Expression '/' Expression   { Op Div $1 $3 }
          | Expression '%' Expression   { Op Mod $1 $3 }
          | true                        { BooleanConst True }
          | false                       { BooleanConst False }


Operation : id '=' Expression { AssignOp $1 $3 }
          | id "++"           { PosIncrement $1 }
          | id "--"           { PosDecrement $1 }
          | "++" id           { PreIncrement $2 }
          | "--" id           { PreDecrement $2 }


StatementOp : id '=' Expression ';'     { AssignStm $1 $3 }
          | id '=' scan_int '(' ')' ';' { AssignScanInt $1 }
          | Type Expressions ';'        { Init $1 $2 }
          | Type id '=' Expression ';'  { Declaration $1 $2 $4 }


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


Type : int     { IntType }
     | bool    { BoolType }
     | string  { StringType }


Declaration : {- empty -}               { [] }
          | Type id                     { [($1, $2)] }
          | Declaration ',' Type id     { $1 ++ [($3, $4)] }


Expressions : {- empty -}              { [] }
          | Expression                 { [$1] }
          | Expressions ',' Expression { $1 ++ [$3] }


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}
