{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

literal                                               { STRING_LIT $$ } 
identifier                                            { ID $$ }
numeral                                               { NUM $$ }


program	                                              { PROGRAM }
function	                                            { FUNCTION }
procedure	                                            { PROCEDURE }
const	                                                { CONST }
var	                                                  { VAR }
begin	                                                { BEGIN }
end  	                                                { END }
if	                                                  { IF }
then	                                                { THEN }
else	                                                { ELSE }
while	                                                { WHILE }
do	                                                  { DO }
for	                                                  { FOR }
to	                                                  { TO }
true	                                                { TRUE }
false	                                                { FALSE }
integer	                                              { INTEGER }
boolean	                                              { BOOLEAN }
string	                                              { STRING }
array	                                                { ARRAY }
of	                                                  { OF }
break	                                                { BREAK }

-- Punctuation Signs
','                                                   { COMMA }
'.'                                                   { DOT }
':'                                                   { COLON }
';'                                                   { SEMICOLON }
'('                                                   { LPAREN }
')'                                                   { RPAREN }
'['                                                   { LBRACKET }
']'                                                   { RBRACKET }

-- Infix Operators
'+'                                                   { PLUS }
'-'                                                   { MINUS }
'*'                                                   { TIMES }
'='                                                   { EQUALS }
'<>'                                                  { DIFFERENT }
'<'                                                   { LESSTHAN }
'<='                                                  { LESSEQUAL }
'>'                                                   { GREATERTHAN }
'>='                                                  { GREATEREQUAL }
'div'                                                 { DIV }
'mod'                                                 { MOD }
'and'                                                 { AND }
'or'                                                  { OR }
'not'                                                 { NOT }
':='                                                  { ASSIGN }


{- Precedences.. -}
-- We left one shift/reduce conflict because we are certain that Happy
-- is doing the right choice.
 
-- Lowest precedence
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left '+' '-' 'or' 
%left '*' 'div' 'mod' 'and'  
%nonassoc 'not'
-- Highest precedence

%%



{- Programs -}

Program : ProgramHeader ProgramBody '.'                                  { StartProgram $1 $2 }

ProgramHeader : program identifier ';'                                   { ProgHead $2 }

ProgramBody : ConstDecls ProcDecls VarDecls begin StmList end            { ProgBody $1 $2 $3 $5 }





{- Procedures -}

ProcDecls : ProcDefSeq                                                           { DeclareProcedures $1 }
          | {- ε -}                                                              { DeclareProcedures [] }
          
ProcDefSeq : Proc ProcDefSeq                                                     { $1 : $2 }
           | Proc                                                                { [$1] }

Proc: ProcHeader ProcBody ';'                                                    { Procedure $1 $2 }

ProcHeader : procedure identifier '(' ParamList ')' ';'                          { ProcedureName $2 $4 }
           | function identifier '(' ParamList ')' ':' BasicType ';'             { FunctionName $2 $4 $7 }

ProcBody: VarDecls begin StmList end                                             { ProcedureBody $1 $3 }
  
ParamList : Param ';' ParamList                                                  { $1 : $3 }
          | Param                                                                { [$1] }
          | {- ε -}                                                               { [] }    -- If we leave this uncommented, it accepts the empty list  
                                                                                               
Param : identifier ':' Type                                                      { Parameter $1 $3 }






{- Statements -}

StmList : Stm ';' StmList                             { $1 : $3 }
        | Stm                                         { [$1] }


Stm : VarAccess ':=' Expr                            { AssignStm $1 $3 }
    | if Expr then Stm                               { IfStm $2 $4 }
    | if Expr then Stm else Stm                      { IfThenElseStm $2 $4 $6 }
    | while Expr do Stm                              { WhileStm $2 $4 }
    | for identifier ':=' Expr to Expr do Stm        { ForStm $2 $4 $6 $8 }
    | break                                          { BreakStm }
    | identifier '(' ExprList ')'                    { ProcedureStm $1 $3 }
    | begin StmList end                              { CompoundStm $2 }








{- Declarations -}

ConstDecls : const ConstDefSeq                           { DeclareConstants $2 }
           | {- ε -}                                     { DeclareConstants [] }

ConstDefSeq : ConstDef ConstDefSeq                       { $1 : $2 }
            | ConstDef                                   { [$1] }

ConstDef : identifier '=' numeral ';'                    { DefineConst $1 $3 }

VarDecls : var VarDefSeq                                 { DeclareVariables $2 }
         | {- ε -}                                       { DeclareVariables [] }

VarDefSeq : VarDef VarDefSeq                             { $1 : $2 }
          | VarDef                                       { [$1] }

VarDef : identifier ':' Type ';'                         { Declare $1 $3 }

Type : BasicType                                         { TB $1 }
     | ArrayType                                         { TA $1 }

BasicType : integer                                      { TypeInteger }
          | boolean                                      { TypeBoolean }
          | string                                       { TypeString }

ArrayType : array '[' Constant '.' '.' Constant ']' of BasicType  { TypeArray $3 $6 $9 } 
          
Constant : numeral                                       { Int $1 }
         | identifier                                    { Const $1 }



{- Expressions -}

Expr : numeral                                         { Num $1 }
     | literal                                         { StringLiteral $1 }
     | true                                            { BooleanTrue }
     | false                                           { BooleanFalse }
     | VarAccess                                       { Access $1 }
     | Expr '+' Expr                                   { BinOp Sum $1 $3 }
     | Expr '-' Expr                                   { BinOp Subtract $1 $3 }
     | Expr '*' Expr                                   { BinOp Multiply $1 $3 }
     | Expr 'div' Expr                                 { BinOp Divide $1 $3 }
     | Expr 'mod' Expr                                 { BinOp Mod $1 $3 }
     | Expr '=' Expr                                   { BinOp Equals $1 $3 }
     | Expr '<>' Expr                                  { BinOp NotEquals $1 $3 }
     | Expr '<' Expr                                   { BinOp LessThan $1 $3 }
     | Expr '>' Expr                                   { BinOp LessThan $3 $1 }
     | Expr '<=' Expr                                  { BinOp LessEqualThan $1 $3 }
     | Expr '>=' Expr                                  { BinOp LessEqualThan $3 $1 }
     | Expr 'and' Expr                                 { BinOp And $1 $3 }
     | Expr 'or' Expr                                  { BinOp Or $1 $3 }
     | '-'Expr                                         { UnaryOp Minus $2 }
     | 'not' Expr                                      { UnaryOp Not $2 }
     | '(' Expr ')'                                    { $2 }
     | identifier '(' ExprList ')'                     { Call $1 $3 } -- Function Call
   

ExprList : Expr ',' ExprList                           { $1 : $3 }
         | Expr                                        { [$1] }
         | {- ε -}                                     { [] }

VarAccess : identifier                                 { Variable $1 }
          | identifier '[' Expr ']'                    { Array $1 $3 }




{
parseError :: [Token] -> a
parseError toks = error ("parse error at " ++ show toks)
}