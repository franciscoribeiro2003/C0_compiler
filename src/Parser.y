{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

-- Palavras Chave
"int"                  { T_Int }
"float"                { T_Float }
"char"                 { T_Char }
"string"               { T_String }
"bool"                 { T_Bool }
"if"                   { T_If }
"else"                 { T_Else }
"while"                { T_While }
"for"                  { T_For }
"return"               { T_Return }
"print_int"            { T_PrintInt }
"print_float"          { T_PrintFloat }
"print_char"           { T_PrintChar }
"print_string"         { T_PrintString }
"true"                 { T_True }
"false"                { T_False }

-- Operadores
"+"                    { T_Plus }
"-"                    { T_Minus }
"*"                    { T_Mult }
"/"                    { T_Div }
"%"                    { T_Mod }
"="                    { T_Assign }
"=="                   { T_Equal }
"!="                   { T_NotEqual }
"<"                    { T_Less }
"<="                   { T_LessEqual }
">"                    { T_Greater }
">="                   { T_GreaterEqual }
"!"                    { T_Not }
"&&"                   { T_And }
"||"                   { T_Or }

-- Delimitadores
"("                    { T_LParen }
")"                    { T_RParen }
"{"                    { T_LBrace }
"}"                    { T_RBrace }
";"                    { T_SemiColon }
","                    { T_Comma }

-- Literais
num    { T_Num $$ }
string { T_String $$ }
ident  { T_Ident $$ }

-- Precedences
%nonassoc "="
%left "|" "&"
%nonassoc "<" ">" "<=" ">=" "<>" "="
%left "+" "-"
%left "*" "/" "%"

-- Programa
-- Programa: ListaDecl { Program $1 }

-- ListaDecl : Decl ListaDecl { $1 : $2 }


%%

-- Type : INT { Int }
--      | STRING { String }
--      | BOOL { Bool }

Exp : Exp "+" Exp     { Add $1 $2 }
    | Exp "-" Exp     { Subtraction $1 $2 }
    | Exp "*" Exp     { Multiplication $1 $2 }
    | Exp "/" Exp     { Division Div $1 $2 }
    | Exp "%" Exp     { Module $1 $2 }
    | Exp "=" Exp     { Equal $1 $2 }
    | Exp "==" Exp    { Compare $1 $2 }
    | Exp "!=" Exp    { NotEquals $1 $2 }
    | Exp "<" Exp     { Less $1 $2}
    | Exp "<=" Exp    { LessEquals $1 $2}
    | Exp ">" Exp     { Greater $1 $2}
    | Exp ">=" Exp    { GreaterEquals $1 $2}
    | Exp "&&" Exp    { And $1 $2}
    | Exp "||" Exp    { Or $1 $2}
