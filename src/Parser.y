{
module Parser where
import Lexer
import AST
}

%name parser
%tokentype { Token }
%error { parseError }

%token

-- Palavras Reservadas
STRING              { STRING }
INT                 { INT }
BOOL                { BOOL }
"True"              { TRUE }
"False"             { FALSE }
if                  { IF }
else                { ELSE }
while               { WHILE }
FOR                 { FOR }
RETURN              { RETURN }
"print_int"         { PRINT }
"print_string"      { PRINT }

-- Operadores
"+"                   { PLUS }
"-"                   { MINUS }
"*"                   { MULT }
"/"                   { DIV }
"%"                   { MOD }
"=="                  { EQUAL }
"!="                  { NEQUAL }
"<"                   { LOWERT }
"<="                  { LOWEREQ }
">"                   { GREATERT }
">="                  { GREATEREQ }
"&&"                  { AND }
"||"                  { OR }
"!"                   { NOT }
"="                   { ASSIGN }

-- Sinais de Pontuação
";"                   { SEMICOLON }
","                   { COMMA }
"("                   { LPAREN }
")"                   { RPAREN }
"{"                   { LBRACE }
"}"                   { RBRACE }

-- Identificadores
ID               { ID $$ }
NUM              { NUM $$ }
STRLIT           { STRLIT $$ }

-- Precedência de Operadores
%nonassoc "="
%left "|" "&"
%nonassoc "<" ">" "<=" ">=" "<>" "="
%left "+" "-"
%left "*" "/" "%"
%right "!"


-- Gramática

%%

expression : ID { Var $1 }
           | NUM { Num $1 }
           | expression "+" expression {Binop Plus $1 $3 }
           | expression "-" expression {Binop Minus $1 $3 }
           | expression "*" expression {Binop Mult $1 $3 }
           | expression "/" expression {Binop Div $1 $3 }
           | expression "%" expression {Binop Mod $1 $3 }
           | expression "==" expression {Binop Equal $1 $3 }
           | expression "!=" expression {Binop Nequal $1 $3 }
           | expression "<" expression {Binop Lowert $1 $3 }
           | expression "<=" expression {Binop Lowereq $1 $3 }
           | expression ">" expression {Binop Greatert $1 $3 }
           | expression ">=" expression {Binop Greatereq $1 $3 }
           | expression "&&" expression {Binop And $1 $3 }
           | expression "||" expression {Binop Or $1 $3 }
           | "!" expression {UnaryExpression Not $2 }
           | "(" expression ")" { $2 }
           --| STRLIT { StrLit $1 }
           --| "True" { BoolLit True }
           --| "False" { BoolLit False }

assignment : ID "=" expression ";" { Assignment $1 $3 }

Type : INT     { IntType }
     | BOOL    { BoolType }
     | STRING  { StringType }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
