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
"if"                { IF }
"else"              { ELSE }
"while"             { WHILE }
"for"               { FOR }
"return"            { RETURN }
"print_int"         { PRINT_INT }
"print_string"      { PRINT_STR }
"scan_int"          { SCAN_INT }
"scan_string"       { SCAN_STR }

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
"++"                  { INCREMENT }
"--"                  { DECREMENT }

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
                
-- UnOp : "!"  { Not }
--      | "++" { Increment }
--      | "--" { Decrement }

Statments : Statment { [$1] }
          | Statments Statment { $1 ++ [$2] }

Statment : Assignment                                                      { Statment $1 }
     | "{" Statments "}"                                                   {Statments $2 }
     | "if" "(" expression ")" Statment                                    { If $3 $5 }
     | "if" "(" expression ")" Statment "else" Statment                    { If_Else $3 $5 $7 }
     | "while" "(" expression ")" Statment                                 { While $3 $5 }
     | "return" expression ";"                                             { Return $2 }
     | "for" "(" AssignmentFor ";" expression ";" AssignmentFor ")" Statment { For $3 $5 $7 $9 }
    --| "{" Statment "}" Statment { BraceStatment $2 $4}     --Statment list

Assignment_List : Assignment Assignment_List { $1 : $2 }
                | Assignment { [$1] }

Assignment : expression { Assign $1 }
         | Type ID ";" { Init $1 $2 }
         | ID "=" expression ";" { SetValue $3 $1}
         | Type ID "=" expression ";" { Declaration $4 $2}
         
         --| ID UnOp { UnOp $1 $2}

AssignmentFor :  { EmptyFor }
         | Type ID "=" expression { InitFor $1 $2 $4 }
         | ID "=" expression { SetValueFor $3 $1}
         | ID "++" { IncrementFor $1 }
         | ID "--" { DecrementFor $1 }

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
           | "(" expression ")" { $2 }
           --| "!" expression {UnaryExpression Not $2 }
           --| STRLIT { StrLit $1 }
           --| "True" { BoolLit True }
           --| "False" { BoolLit False }
          


Type : INT     { IntType }
     | BOOL    { BoolType }
     | STRING  { StringType }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}