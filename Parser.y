{
module Parser (parser) where
import Types
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  while             { While _ }
  do                { Do _ }
  '['               { BracketOpen _ }
  ']'               { BracketClose _ }
  '('               { ParensOpen _ }
  ')'               { ParensClose _ }
  ';'               { Semicolon _ }
  ':='              { Assign _ }
  sym               { Sym _ $$ }
  var               { Var _ $$ }
  int               { Int _ $$ }
  '<'               { TLT _ }
  '>'               { TGT _ }
  '&&'              { TAND _ }
%%


program : statements                    { Program $1 }
statements : {- empty -}                { [] }
           | statements statement       { $2 : $1 }


statement : var ':=' expr ';'   { Sass $1 $3 }
          | var ':=' expr       { Sass $1 $3 }
          | while exprb do '(' statements ')' { Swhile $2 $5 }

expr : expr1                            { $1 }
     | expr sym expr1                   { Aop $2 $1 $3 }

expr1 : var                             { Variable $1 }
      | '(' expr ')'                    { $2 }

exprb : exprb1 '&&' exprb                { Band $1 $3 }
      | exprb1                           { $1 }

exprb1 : expr '<' expr                   { Bleq $1 $3 }

{


parseError :: [Token] -> a
parseError _ = error "Parse error"

}
