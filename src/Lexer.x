{
module Lexer where
}

%wrapper "basic"

$digit     = [0-9]
$graphic   = $printable
$white     = [\ \t\n\f\v]

@String    = \" ($graphic # \")*\"
@id        = [A-Za-z_][A-Za-z0-9_]*

tokens :-

    -- ignore white
    $white+                  ;

    -- ignore single line comments
    "//".*                                         ;

    -- ignore multiline comments
    "/*"([^\*]|[\n]|("*"+([^\*\/]|[\n])))*"*"+"/"  ;

    -- keywords
    string                  { \_ -> STRING_DEF_TOKEN }
    true                    { \s -> TRUE_TOKEN True }
    false                   { \s -> FALSE_TOKEN False }
    if                      { \_ -> IF_TOKEN }
    else                    { \_ -> ELSE_TOKEN }
    while                   { \_ -> WHILE_TOKEN }
    for                     { \_ -> FOR_TOKEN }
    return                  { \_ -> RETURN_TOKEN }
    int                     { \_ -> INT_DEF_TOKEN }
    bool                    { \_ -> BOOL_DEF_TOKEN }
    scan_int                { \_ -> SCANINT_TOKEN }
    print_int               { \_ -> PRINTINT_TOKEN }
    print_str               { \_ -> PRINTSTR_TOKEN }

    -- tokens
    $digit+                 { \s -> NUM_TOKEN (read s) }
    @String                 { \s -> STRING_TOKEN(read s) }
    @id                     { \s -> VAR_TOKEN s }

    -- operators
    "++"                    { \_ -> INCR_TOKEN }
    "--"                    { \_ -> DECR_TOKEN }
    "+"                     { \_ -> PLUS_TOKEN }
    "-"                     { \_ -> MINUS_TOKEN }
    "*"                     { \_ -> MULT_TOKEN }
    "/"                     { \_ -> DIV_TOKEN }
    "%"                     { \_ -> MOD_TOKEN }
    "("                     { \_ -> LPAREN_TOKEN }
    ")"                     { \_ -> RPAREN_TOKEN }
    "{"                     { \_ -> LBRACE_TOKEN }
    "}"                     { \_ -> RBRACE_TOKEN }
    "="                     { \_ -> ASSIGN_TOKEN }
    "=="                    { \_ -> EQUAL_TOKEN }
    "!="                    { \_ -> NEQUAL_TOKEN }
    "<="                    { \_ -> LTOE_TOKEN }
    "<"                     { \_ -> LTHEN_TOKEN }
    ">="                    { \_ -> GTOE_TOKEN }
    ">"                     { \_ -> GTHEN_TOKEN }
    ";"                     { \_ -> SEMICOLON_TOKEN }
    ","                     { \_ -> COLON_TOKEN }
    "&&"                    { \_ -> AND_TOKEN }
    "||"                    { \_ -> OR_TOKEN }
    "!"                     { \_ -> NOT_TOKEN}

{
data Token
  = NUM_TOKEN Int
  | STRING_TOKEN String
  | TRUE_TOKEN Bool
  | FALSE_TOKEN Bool
  | VAR_TOKEN String
  | PLUS_TOKEN
  | MINUS_TOKEN
  | MULT_TOKEN
  | DIV_TOKEN
  | MOD_TOKEN
  | LPAREN_TOKEN
  | RPAREN_TOKEN
  | LBRACE_TOKEN
  | RBRACE_TOKEN
  | IF_TOKEN
  | ELSE_TOKEN
  | WHILE_TOKEN
  | FOR_TOKEN
  | PRINTINT_TOKEN
  | PRINTSTR_TOKEN
  | SCANINT_TOKEN
  | STRING_DEF_TOKEN
  | INCR_TOKEN
  | DECR_TOKEN
  | ASSIGN_TOKEN
  | LTHEN_TOKEN
  | GTHEN_TOKEN
  | LTOE_TOKEN
  | GTOE_TOKEN
  | EQUAL_TOKEN
  | NEQUAL_TOKEN
  | SEMICOLON_TOKEN
  | COLON_TOKEN
  | RETURN_TOKEN
  | NOT_TOKEN
  | AND_TOKEN
  | OR_TOKEN
  | INT_DEF_TOKEN
  | BOOL_DEF_TOKEN
  deriving (Eq, Show)
}
