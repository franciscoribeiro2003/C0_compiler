{
module Lexer where
import AST
}

%wrapper "basic"

$white = [\ \t\n\r\v]
$digit = [0-9]
$alpha = [a-zA-Z]
$underscore = [_]

tokens :-

  $white+ ;

  -- Palavras Chave
  "int"                  { \_ -> T_Int }
  "string"               { \_ -> T_String }
  "bool"                 { \_ -> T_Bool }
  "if"                   { \_ -> T_If }
  "else"                 { \_ -> T_Else }
  "while"                { \_ -> T_While }
  "for"                  { \_ -> T_For }
  "return"               { \_ -> T_Return }
  "print_int"            { \_ -> T_PrintInt }
  "print_string"         { \_ -> T_PrintString }
  "true"                 { \_ -> T_True }
  "false"                { \_ -> T_False }

  -- Operadores
  "+"                    { \_ -> T_Plus }
  "-"                    { \_ -> T_Minus }
  "*"                    { \_ -> T_Mult }
  "/"                    { \_ -> T_Div }
  "%"                    { \_ -> T_Mod }
  "="                    { \_ -> T_Assign }
  "=="                   { \_ -> T_Equal }
  "!="                   { \_ -> T_NotEqual }
  "<"                    { \_ -> T_Less }
  "<="                   { \_ -> T_LessEqual }
  ">"                    { \_ -> T_Greater }
  ">="                   { \_ -> T_GreaterEqual }
  "!"                    { \_ -> T_Not }
  "&&"                   { \_ -> T_And }
  "||"                   { \_ -> T_Or }

  -- Delimitadores
  "("                    { \_ -> T_LParen }
  ")"                    { \_ -> T_RParen }
  "{"                    { \_ -> T_LBrace }
  "}"                    { \_ -> T_RBrace }
  ";"                    { \_ -> T_SemiColon }
  ","                    { \_ -> T_Comma }

  -- Identificadores
  $alpha($alpha|$digit|$underscore)* { \s -> T_Ident s }

  -- Literais
  $digit+                { \s -> T_Num (read s) }
  "\"([^\"\\]|\\.)*\""   { \s -> T_String (read s) }

  -- Comentários
  "//".*          ;
  "/*"(.|\n)*"*/" ;
{
data Token
  =  T_If
    | T_Int
    | T_Else
    | T_While
    | T_For
    | T_Return
    | T_PrintInt
    | T_PrintString
    | T_Plus
    | T_Minus
    | T_Mult
    | T_Div
    | T_Mod
    | T_Assign
    | T_Equal
    | T_NotEqual
    | T_Less
    | T_LessEqual
    | T_Greater
    | T_GreaterEqual
    | T_Not
    | T_And
    | T_Or
    | T_LParen
    | T_RParen
    | T_LBrace
    | T_RBrace
    | T_SemiColon
    | T_Comma
    | T_False
    | T_True
    | T_Bool
    | T_Num Int
    | T_Ident String
    | T_String String
    deriving (Eq, Show)
}
