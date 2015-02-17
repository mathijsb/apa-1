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
                                       , Stmnt_ (RootSet, StmntL, Assgn, IfThenElse, While, Skip)
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
parser = C.parser lexer (pStmnt <* eof)

-- | Parses a 'IntExpr'.
pIntExpr :: TokenParser IntExpr
pIntExpr = (\i -> IEInt i) <$> nat
       <|> (\i -> IEVar i) <$> var
-- <|> (\a b -> IEOp a Plus b) <$> pIntExpr <* spec '+' <*> pIntExpr
-- <|> (\a b -> IEOp a Minus b) <$> pIntExpr <* spec '-' <*> pIntExpr

-- | Parses a 'BoolExpr'.
pBoolExpr :: TokenParser BoolExpr
pBoolExpr = (BEBool True) <$ keyword "true"
        <|> (BEBool False) <$ keyword "false"
        <|> (\b -> BENot b) <$ keyword "not" <*> pBoolExpr
-- <|> (\a b -> BEOp a And b) <$> pBoolExpr <* keyword "and" <*> pBoolExpr
-- <|> (\a b -> BEOp a Or b) <$> pBoolExpr <* keyword "or" <*> pBoolExpr
-- <|> (\a b -> BEInt a EQ b) <$> pIntExpr <* spec '=' <* spec '=' <*> pIntExpr

-- | Parses a 'Stmnt'.
pStmnt :: TokenParser Stmnt
pStmnt = (\pos ss -> Stmnt pos (RootSet ss)) <$> sourcePos <*> some pStmnt'

pStmnt' :: TokenParser Stmnt
pStmnt' = (\pos ss                       -> Stmnt pos (StmntL ss)) <$>
           sourcePos <* spec '{' <*> many pStmnt <* spec '}'
     <|> (\pos ss                       -> Stmnt pos (StmntL ss)) <$>
           sourcePos <* spec '{' <*> many pStmnt <* spec '}'
     <|> (\pos x ie                     -> Stmnt pos (Assgn x ie)) <$>
           sourcePos <*> var <* spec ':' <* spec '=' <*> pIntExpr <* spec ';'
     <|> (\pos                          -> Stmnt pos (Skip)) <$>
           sourcePos <* keyword "skip" <* spec ';'
     <|> (\pos cond thenBody elseBody   -> Stmnt pos (IfThenElse cond thenBody elseBody)) <$> -- TODO: If without else
           sourcePos <* keyword "if" <* spec '(' <*> pBoolExpr <* spec ')' <*
           keyword "then" <*> pStmnt <*
           keyword "else" <*> pStmnt
     <|> (\pos cond body                -> Stmnt pos (While cond body)) <$>
           sourcePos <* keyword "while" <* spec '(' <*> pBoolExpr <* spec ')' <*
           keyword "do" <*> pStmnt
