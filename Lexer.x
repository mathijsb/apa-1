{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-
  $white+                         ;
  "--".*                          ;
  while                           { tok (\p s -> While p) }
  do                              { tok (\p s -> Do p) }
  :=                              { tok (\p s -> Assign p) }

  \[                              { tok (\p s -> BracketOpen p) }
  \]                              { tok (\p s -> BracketClose p) }
  \(                              { tok (\p s -> ParensOpen p) }
  \)                              { tok (\p s -> ParensClose p) }
  \;                              { tok (\p s -> Semicolon p) }

  $digit+                         { tok (\p s -> Int p (read s)) }
  [\=\+\-\*\/\<\>]                { tok (\p s -> Sym p (head s)) }
  $alpha [$alpha $digit \_ \']*   { tok (\p s -> Var p s) }

{

-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token =
  While AlexPosn |
  Do AlexPosn |
  BracketOpen AlexPosn |
  BracketClose AlexPosn |
  ParensOpen AlexPosn |
  ParensClose AlexPosn |  
  Semicolon AlexPosn |
  Assign AlexPosn |
  Sym AlexPosn Char |
  Var AlexPosn String |
  Int AlexPosn Int
  deriving (Eq,Show)

token_posn (While p) = p
token_posn (Do p) = p
token_posn (Assign p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
token_posn (Semicolon p) = p
token_posn (BracketOpen p) = p
token_posn (BracketClose p) = p
token_posn (ParensOpen p) = p
token_posn (ParensClose p) = p

}