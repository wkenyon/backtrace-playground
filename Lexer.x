{
module Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$lower = a-z
$upper = A-Z

tokens :-
  $white+ 				;
  "--".*				;
  let					{ \s -> Let }
  in					{ \s -> In }
  case                                  { \s -> Case }
  of                                    { \s -> Of }
  push                                  { \s -> Push }
  break                                 { \s -> Break }
  $digit+				{ \s -> Int (read s) }
  [\;\.\\\=\+\-\*\/\(\)\{\}]		{ \s -> Sym (head s) }
  $lower [$alpha $digit \_ \']*		{ \s -> Var s }
  $upper [$alpha $digit \_ \']*         { \s -> Cons s }

{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token
 = Let
 | In
 | Case
 | Of
 | Push
 | Break
 | Sym Char
 | Var String
 | Cons String
 | Int Int
 deriving (Eq,Show)

}
