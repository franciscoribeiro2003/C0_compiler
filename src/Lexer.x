{
module Lexer where 
}

%wrapper "basic"

$digit = [0-9]
$letter = [a-zA-Z]
$underscore = _
$whitespace = [\ \t\n\r\v]

tokens :-
    $whitespace+          ;

    -- keywords
    "string"              { \_ -> STRING }
    "int"                 { \_ -> INT }
    "bool"                { \_ -> BOOL }
    "true"                { \_ -> TRUE }
    "false"               { \_ -> FALSE }
    "if"                  { \_ -> IF }
    "else"                { \_ -> ELSE }
    "while"               { \_ -> WHILE }
    "for"                 { \_ -> FOR }
    "return"              { \_ -> RETURN }
    "print_int"           { \_ -> PRINT }
    "print_string"        { \_ -> PRINT }

    -- operators
    "+"                   { \_ -> PLUS }
    "-"                   { \_ -> MINUS }
    "*"                   { \_ -> MULT }
    "/"                   { \_ -> DIV }
    "%"                   { \_ -> MOD }
    "=="                  { \_ -> EQUAL }
    "!="                  { \_ -> NEQUAL }
    "<"                   { \_ -> LOWERT }
    "<="                  { \_ -> LOWEREQ }
    ">"                   { \_ -> GREATERT }
    ">="                  { \_ -> GREATEREQ }
    "&&"                  { \_ -> AND }
    "||"                  { \_ -> OR }
    "!"                   { \_ -> NOT }
    "="                   { \_ -> ASSIGN }
    "++"                  { \_ -> INCREMENT }
    "--"                  { \_ -> DECREMENT }

    -- delimiters
    ";"                   { \_ -> SEMICOLON }
    ","                   { \_ -> COMMA }
    "("                   { \_ -> LPAREN }
    ")"                   { \_ -> RPAREN }
    "{"                   { \_ -> LBRACE }
    "}"                   { \_ -> RBRACE }

    -- identifiers
    $letter($letter|$digit|$underscore)* { \s -> ID s }

    -- numbers
    $digit+               { \s -> NUM (read s) }

    -- strings
    "\"([^\"\\]|\\.)*\""  { \s -> STRLIT (read s) }

    -- comments
    "//".*                ;
    "/*"(.|\n)*"*/"       ;


{
data Token
    = STRING
    | INT
    | BOOL
    | TRUE
    | FALSE
    | IF
    | ELSE
    | WHILE
    | FOR
    | RETURN
    | PRINT
    | PLUS
    | MINUS
    | MULT
    | DIV
    | MOD
    | EQUAL
    | NEQUAL
    | LOWERT
    | LOWEREQ
    | GREATERT
    | GREATEREQ
    | INCREMENT
    | DECREMENT
    | AND
    | OR
    | NOT
    | ASSIGN
    | SEMICOLON
    | COMMA
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | ID String
    | NUM Int
    | STRLIT String
    deriving (Show, Eq)
}
