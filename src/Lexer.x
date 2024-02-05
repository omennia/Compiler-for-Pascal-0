{
module Lexer where
import Data.Char
import Data.Map (fromList, lookup) 
}

%wrapper "basic"
$alpha = [_a-zA-Z]
$digit = [0-9]
$anything = [. \n]
@string_literal = \' ( $anything # \' )* \' 
@multi_line_comment = "(*"  ($white | [^\*] | "*"+( [^\*\)] | $white)   )* \*+")"

tokens :-
    -- White space and others
    $white+                                                 ;

    -- Punctuation Signs
    ","                                                     { \_ -> COMMA }
    "."                                                     { \_ -> DOT }
    ":"                                                     { \_ -> COLON }
    ";"                                                     { \_ -> SEMICOLON }
    "("                                                     { \_ -> LPAREN }
    ")"                                                     { \_ -> RPAREN }
    "["                                                     { \_ -> LBRACKET }
    "]"                                                     { \_ -> RBRACKET }

    -- Infix Operators
    "+"                                                     { \_ -> PLUS }
    "-"                                                     { \_ -> MINUS }
    "*"                                                     { \_ -> TIMES }
    "="                                                     { \_ -> EQUALS }
    "<>"                                                    { \_ -> DIFFERENT }
    "<"                                                     { \_ -> LESSTHAN }
    "<="                                                    { \_ -> LESSEQUAL }
    ">"                                                     { \_ -> GREATERTHAN }
    ">="                                                    { \_ -> GREATEREQUAL }
    "div"                                                   { \_ -> DIV }
    "mod"                                                   { \_ -> MOD }
    "and"                                                   { \_ -> AND }
    "or"                                                    { \_ -> OR }
    "not"                                                   { \_ -> NOT }
    ":="                                                    { \_ -> ASSIGN }
    
    @string_literal                                         { \s -> STRING_LIT (filter (/= '\'') s) }    
    
    $alpha($alpha|$digit)*                                  { \s -> case isSpecialKeyword (map toLower s) of
                                                                        Just x -> x
                                                                        Nothing -> ID (map toUpper s) }
                                                                        
    $digit+                                                 { \s -> NUM (read s) }

    @multi_line_comment                                     ;
{
data Token = 
          ID String       
        | NUM Int         
        | STRING_LIT String

        -- Punctuation signs
        | COMMA | DOT | COLON | SEMICOLON
        | LPAREN | RPAREN | LBRACKET | RBRACKET

        -- Infix operators
        | PLUS | MINUS | TIMES | EQUALS
        | DIFFERENT | LESSTHAN | LESSEQUAL
        | GREATERTHAN | GREATEREQUAL | DIV
        | MOD | AND | OR | NOT | ASSIGN

        -- Reserved keywords 
        | PROGRAM | FUNCTION | PROCEDURE | CONST | VAR | BEGIN
        | IF | THEN | ELSE | WHILE | DO | FOR | TO | TRUE | FALSE
        | INTEGER | BOOLEAN | STRING | ARRAY | OF | BREAK | END

        -- IO operations (also reserved keywords)
        | READINT | WRITEINT | WRITESTR
        deriving (Eq,Show)

{- Case insensitive special words -}
specialWords = Data.Map.fromList([("program", PROGRAM), ("function", FUNCTION), ("procedure", PROCEDURE), ("const", CONST), ("var", VAR), 
                                  ("begin", BEGIN), ("and", AND), ("if", IF), ("then", THEN), ("else", ELSE), ("while", WHILE), ("do", DO),
                                  ("for", FOR), ("to", TO), ("true", TRUE), ("false", FALSE), ("div", DIV), ("mod", MOD), ("integer", INTEGER), 
                                  ("boolean", BOOLEAN), ("string", STRING), ("array", ARRAY), ("of", OF), ("break", BREAK), ("end", END)])

isSpecialKeyword :: String -> Maybe Token
isSpecialKeyword s = Data.Map.lookup s specialWords
}