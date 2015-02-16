module CCO.ExWhile (
    -- * Syntax
    Label
  , Var
  , IntOp (Plus, Minus)
  , BoolOp (And, Or)
  , IntComp (EQ, LT, LTE, GT, GTE)
  , IntExpr (IEInt, IEVar, IEOp)
  , BoolExpr (BEBool, BENot, BEOp, BEInt)
  , Stmnt (Stmnt)                                    -- instances: Tree
  , Stmnt_ (Assgn, IfThenElse, While, Skip, StmntL)  -- instances: Tree

    -- * Parser
  , parser                      -- :: Component String Tm
) where

import CCO.ExWhile.Base      ( Label
                             , Var
                             , IntOp (Plus, Minus)
                             , BoolOp (And, Or)
                             , IntComp (EQ, LT, LTE, GT, GTE)
                             , IntExpr (IEInt, IEVar, IEOp)
                             , BoolExpr (BEBool, BENot, BEOp, BEInt)
                             , Stmnt (Stmnt)                                    -- instances: Tree
                             , Stmnt_ (Assgn, IfThenElse, While, Skip, StmntL)  -- instances: Tree
                             )
import CCO.ExWhile.Parser    (parser)
