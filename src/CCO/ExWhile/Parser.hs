module CCO.ExWhile.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import CCO.ExWhile.Base as XW
import CCO.ExWhile.Lexer               (Token, lexer, keyword, var, nat, str, spec)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-- | Helper type for parsing procedure signatures.
type ArgsAndRefs = (VarL, VarL)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing terms.
parser :: Component String Stmnt
parser = C.parser lexer (pRootStmnt <* eof)

-- | Parses a 'IntExpr'.
pIntOp :: TokenParser IntOp
pIntOp = Plus <$ spec '+'
     <|> Minus <$ spec '-'
     <|> Times <$ spec '*'
     <|> Div <$ spec '/'
     <|> Modulo <$ spec '%'

pIntExpr :: TokenParser IntExpr
pIntExpr = pSimpleIntExpr
       <|> spec '(' *> pIntExpr <* spec ')'
       <|> (\a op b -> IEOp a op b) <$> pSimpleIntExpr <*> pIntOp <*> pIntExpr

pSimpleIntExpr :: TokenParser IntExpr
pSimpleIntExpr = (\i -> IEInt i) <$> nat
             <|> (\i -> IEVar i) <$> var

-- | Parses a 'BoolExpr'.
pBoolOp :: TokenParser BoolOp
pBoolOp = And <$ keyword "and"
      <|> Or <$ keyword "or"
      <|> Nand <$ keyword "nand"
      <|> Xor <$ keyword "xor"

pIntComp :: TokenParser IntComp
pIntComp = XW.EQ <$ spec '=' <* spec '='
       <|> XW.LTE <$ spec '<' <* spec '='
       <|> XW.LT <$ spec '<'
       <|> XW.GTE <$ spec '>' <* spec '='
       <|> XW.GT <$ spec '>'

pBoolExpr :: TokenParser BoolExpr
pBoolExpr = pSimpleBoolExpr
        <|> spec '(' *> pBoolExpr <* spec ')'
        <|> (\a op b -> BEOp a op b) <$> pSimpleBoolExpr <*> pBoolOp <*> pBoolExpr

pSimpleBoolExpr :: TokenParser BoolExpr
pSimpleBoolExpr = (BEBool True) <$ keyword "true"
              <|> (BEBool False) <$ keyword "false"
              <|> (\b -> BENot b) <$ keyword "not" <*> pBoolExpr
              <|> (\a comp b -> BEInt a comp b) <$> pIntExpr <*> pIntComp <*> pIntExpr

-- | Parses a 'Proc'.
pProcArgOrRef :: TokenParser ArgsAndRefs
pProcArgOrRef = (\arg -> ([arg], [])) <$ keyword "arg" <*> var
            <|> (\ref -> ([], [ref])) <$ keyword "ref" <*> var

pProcSignature :: TokenParser ArgsAndRefs
pProcSignature = (\xs -> collect xs) <$> manySepBy (spec ',') pProcArgOrRef
  where collect xs = (foldl (++) [] (map fst xs), foldl (++) [] (map snd xs))

pProc :: TokenParser Proc
pProc = (\name (args, refs) body -> Proc name args refs body) <$
          keyword "proc" <*> var <*
          spec '(' <*> pProcSignature <* spec ')' <*
          keyword "is" <*> pStmnt <* keyword "end"

-- | Parses a 'Stmnt'.
pRootStmnt :: TokenParser Stmnt
pRootStmnt = (\pos procs ss -> Stmnt pos (RootSet procs ss)) <$>
               sourcePos <*> many pProc <*> someSepBy (spec ';') pStmnt

pStmnt :: TokenParser Stmnt
pStmnt = pStmnt' <* many (spec ';')

pStmnt' :: TokenParser Stmnt
pStmnt' = (\pos s ss                     -> Stmnt pos (StmntL (s:ss))) <$>
            sourcePos <* spec '(' <*> pStmnt <*> many (spec ';' *> pStmnt) <* spec ')'
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
