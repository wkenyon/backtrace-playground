{
module Parser (parse) where
import Syntax
import Lexer
}

%tokentype { Token }
%token
 'let' { Let }
 'in'  { In }
 'case' { Case }
 'of'  { Of }
 'push' { Push }
 'break' { Break }
 '+'   { Sym '+' }
 '='   { Sym '=' }
 ';'   { Sym ';' }
 '('   { Sym '(' }
 ')'   { Sym ')' }
 '\\'  { Sym '\\' }
 '.'   { Sym '.' }
 '{'   { Sym '{' }
 '}'   { Sym '}' }
 VAR   { Var $$ }
 INT   { Int $$ }
 CONS  { Cons $$ }
%name parse

%%

prog	:: { [Bind1] }
 	: {- empty -}		{ [] }
	| bind ';' prog		{ $1 : $3 }

bind 	:: { Bind1 }
	: VAR '=' expr		{ ($1, $3) }

cons    :: { Cons }
        : CONS varlist          { ($1, $2) }
        
varlist :: { [Var] }
        : {- empty -}           { [] }
        | VAR varlist           { $1 : $2 }

alts    :: { [Alt1] }
        : '{' alts2 '}'         { $2 }

alts2   :: { [Alt1] }
        : {- empty -}           { [] }
        | cons '.' expr ';' alts2  { ($1,$3) : $5 }

expr	:: { Expr1 }
	: expr0 '+' expr	{ EPlus1 $1 $3 }
	| 'let' bind 'in' expr	{ ELet1 $2 $4 }
        | 'case' expr 'of' alts { ECase1 $2 $4 }
        | 'push' VAR expr       { EPush1 $2 $3 }
	| 'break' expr          { EBreak1 $2 }
        | '\\' VAR '.' expr	{ ELam1 $2 $4 }
        | cons                  { ECons1 $1 }
        | expr0                 { $1 }

expr0 :: { Expr1 } 
      : expr0 expr1             { EApp1 $1 $2 }
      | expr1                   { $1 }

expr1	:: { Expr1 }
        : VAR			{ EVar1 $1 }
	| INT			{ EInt1 $1 }
	| '(' expr ')'		{ $2 }

{
happyError = error "parse error"
}
