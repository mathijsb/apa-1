module CCO.ExWhile.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import Prelude hiding (EQ, LT, LTE, GT, GTE)
import CCO.ExWhile.Base                ( Label
                                       , Var
                                       , IntOp (Plus, Minus)
                                       , BoolOp (And, Or)
                                       , IntComp (EQ, LT, LTE, GT, GTE)
                                       , IntExpr (IEInt, IEVar, IEOp)
                                       , BoolExpr (BEBool, BENot, BEOp, BEInt)
                                       , Stmnt (Stmnt)
                                       , Stmnt_ (RootSet, StmntL, Assgn, IfThen, IfThenElse, While, Skip)
                                       )
import CCO.ExWhile.Lexer               (Token, lexer, keyword, var, nat, str, spec)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing terms.
parser :: Component String Stmnt
parser = C.parser lexer (pRootStmnt <* eof)

-- | Parses a 'IntExpr'.
pIntExpr :: TokenParser IntExpr
pIntExpr = pSimpleIntExpr
       <|> spec '(' *> pIntExpr <* spec ')'
       <|> (\a b -> IEOp a Plus b) <$> pSimpleIntExpr <* spec '+' <*> pIntExpr
       <|> (\a b -> IEOp a Minus b) <$> pSimpleIntExpr <* spec '-' <*> pIntExpr

pSimpleIntExpr :: TokenParser IntExpr
pSimpleIntExpr = (\i -> IEInt i) <$> nat
             <|> (\i -> IEVar i) <$> var

-- | Parses a 'BoolExpr'.
pBoolExpr :: TokenParser BoolExpr
pBoolExpr = pSimpleBoolExpr
        <|> spec '(' *> pBoolExpr <* spec ')'
        <|> (\a b -> BEOp a And b) <$> pSimpleBoolExpr <* keyword "and" <*> pBoolExpr
        <|> (\a b -> BEOp a Or b) <$> pSimpleBoolExpr <* keyword "or" <*> pBoolExpr

pSimpleBoolExpr :: TokenParser BoolExpr
pSimpleBoolExpr = (BEBool True) <$ keyword "true"
              <|> (BEBool False) <$ keyword "false"
              <|> (\b -> BENot b) <$ keyword "not" <*> pBoolExpr
              <|> (\a b -> BEInt a EQ b) <$> pIntExpr <* spec '=' <* spec '=' <*> pIntExpr

-- | Parses a 'Stmnt'.
pRootStmnt :: TokenParser Stmnt
pRootStmnt = (\pos s ss -> Stmnt pos (RootSet (s:ss))) <$>
               sourcePos <*> pStmnt <*> many (spec ';' *> pStmnt)

pStmnt :: TokenParser Stmnt
pStmnt = pStmnt' <* many (spec ';')

pStmnt' :: TokenParser Stmnt
pStmnt' = (\pos s ss                     -> Stmnt pos (StmntL (s:ss))) <$>
            sourcePos <* spec '{' <*> pStmnt <*> many (spec ';' *> pStmnt) <* spec '}'
      <|> (\pos x ie                     -> Stmnt pos (Assgn x ie)) <$>
            sourcePos <*> var <* spec ':' <* spec '=' <*> pIntExpr
      <|> (\pos                          -> Stmnt pos (Skip)) <$>
            sourcePos <* keyword "skip"
      <|> (\pos cond thenBody elseBody   -> Stmnt pos (IfThenElse cond thenBody elseBody)) <$>
            sourcePos <* keyword "if" <*> pBoolExpr <*
            keyword "then" <*> pStmnt <*
            keyword "else" <*> pStmnt
      <|> (\pos cond body                -> Stmnt pos (IfThen cond body)) <$>
            sourcePos <* keyword "if" <*> pBoolExpr <*
            keyword "then" <*> pStmnt
      <|> (\pos cond body                -> Stmnt pos (While cond body)) <$>
            sourcePos <* keyword "while" <*> pBoolExpr <*
            keyword "do" <*> pStmnt
