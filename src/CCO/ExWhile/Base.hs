module CCO.ExWhile.Base (
    -- * Syntax
    Label
  , Var
  , IntOp (Plus, Minus)
  , BoolOp (And, Or)
  , IntComp (EQ, LT, LTE, GT, GTE)
  , IntExpr (IEInt, IEVar, IEOp)
  , BoolExpr (BEBool, BENot, BEOp, BEInt)
  , Stmnt (Stmnt)
  , Stmnt_ (RootSet, StmntL, Assgn, IfThenElse, While, Skip)
) where

import Prelude hiding (EQ, LT, LTE, GT, GTE)
import CCO.ExWhile.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App, String, Integer))
import CCO.Tree.Parser            (parseTree, app, arg, string)
import Control.Applicative        (Applicative ((<*>)), (<$>), pure)

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Stmnt where
  fromTree (Stmnt pos t) = T.App "Stmnt" [fromTree pos, fromTree t]
  toTree = parseTree [app "Stmnt" (Stmnt <$> arg <*> arg)]

instance Tree Stmnt_ where
  fromTree (RootSet ss)     = T.App "RootSet" [ fromTree ss ]
  fromTree (Assgn x ie)     = T.App "Assgn" [ fromTree x
                                            , fromTree ie ]
  fromTree (IfThenElse cond thenBody elseBody)
                            = T.App "IfThenElse" [ fromTree cond
                                                 , fromTree thenBody
                                                 , fromTree elseBody ]
  fromTree (While cond body)
                            = T.App "While" [ fromTree cond
                                            , fromTree body ]
  fromTree (Skip)           = T.App "Skip" [ ]
  fromTree (StmntL ss)      = T.App "StmntL" [ fromTree ss ]

  toTree = parseTree [ app "RootSet"    (RootSet    <$> arg)
                     , app "Assgn"      (Assgn      <$> arg <*> arg)
                     , app "IfThenElse" (IfThenElse <$> arg <*> arg <*> arg)
                     , app "While"      (While      <$> arg <*> arg)
                     , app "Skip"       (pure Skip)
                     , app "StmntL"     (StmntL     <$> arg)
                     ]

instance Tree IntExpr where
  fromTree (IEInt i)     = T.App "IntExpr:Int" [fromTree i]
  fromTree (IEVar v)     = T.App "IntExpr:Var" [fromTree v]
  fromTree (IEOp a op b) = T.App "IntExp:Op" [fromTree a, fromTree op, fromTree b]

  toTree = parseTree [ app "IntExpr:Int" (IEInt <$> arg)
                     , app "IntExpr:Var" (IEVar <$> arg)
                     , app "IntExpr:Op"  (IEOp  <$> arg <*> arg <*> arg)
                     ]

instance Tree BoolExpr where
  fromTree (BEBool b)     = T.App "BoolExpr:Bool" [fromTree b]
  fromTree (BENot e)      = T.App "BoolExpr:Not" [fromTree e]
  fromTree (BEOp a op b)  = T.App "BoolExpr:Op" [fromTree a, fromTree op, fromTree b]
  fromTree (BEInt a op b) = T.App "BoolExpr:Int" [fromTree a, fromTree op, fromTree b]

  toTree = parseTree [ app "BoolExpr:Bool" (BEBool <$> arg)
                     , app "BoolExpr:Not"  (BENot  <$> arg)
                     , app "BoolExpr:Op"   (BEOp   <$> arg <*> arg <*> arg)
                     , app "BoolExpr:Int"  (BEInt  <$> arg <*> arg <*> arg)
                     ]

instance Tree IntOp where
  fromTree Plus  = T.App "Plus" []
  fromTree Minus = T.App "Minus" []

  toTree = parseTree [ app "Plus"  (pure Plus)
                     , app "Minus" (pure Minus)
                     ]

instance Tree BoolOp where
  fromTree And = T.App "And" []
  fromTree Or  = T.App "Or" []

  toTree = parseTree [ app "And" (pure And)
                     , app "Or"  (pure Or)
                     ]

instance Tree IntComp where
  fromTree EQ  = T.App "EQ" []
  fromTree LT  = T.App "LT" []
  fromTree LTE = T.App "LTE" []
  fromTree GT  = T.App "GT" []
  fromTree GTE = T.App "GTE" []


  toTree = parseTree [ app "EQ"  (pure EQ)
                     , app "LT"  (pure LT)
                     , app "LTE" (pure LTE)
                     , app "GT"  (pure GT)
                     , app "GTE" (pure GTE)
                     ]
