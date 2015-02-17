

-- UUAGC 0.9.51 (src/CCO/ExWhile/AG.ag)
module CCO.ExWhile.AG where

import CCO.Component

{-# LINE 2 "src/CCO/ExWhile/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 12 "src/CCO/ExWhile/AG.hs" #-}
{-# LINE 10 "src/CCO/ExWhile/AG/Base.ag" #-}


type Label = String

type Var = String

data IntOp    = Plus | Minus -- | Times | DivideBy | Modulo
  deriving Show

data BoolOp   = And | Or -- | Nand | Xor
  deriving Show
data IntComp  = EQ | LT | LTE | GT | GTE
  deriving Show

data IntExpr  = IEInt  Integer
              | IEVar  Var
              | IEOp   IntExpr  IntOp  IntExpr

data BoolExpr = BEBool  Bool
              | BENot   BoolExpr
              | BEOp    BoolExpr  BoolOp    BoolExpr
              | BEInt   IntExpr   IntComp   IntExpr

{-# LINE 37 "src/CCO/ExWhile/AG.hs" #-}

{-# LINE 8 "src/CCO/ExWhile/AG.ag" #-}


{-# LINE 42 "src/CCO/ExWhile/AG.hs" #-}
-- Stmnt -------------------------------------------------------
data Stmnt = Stmnt (SourcePos) (Stmnt_)
-- cata
sem_Stmnt :: Stmnt ->
             T_Stmnt
sem_Stmnt (Stmnt _pos _t) =
    (sem_Stmnt_Stmnt _pos (sem_Stmnt_ _t))
-- semantic domain
type T_Stmnt = ( Stmnt)
data Inh_Stmnt = Inh_Stmnt {}
data Syn_Stmnt = Syn_Stmnt {self_Syn_Stmnt :: Stmnt}
wrap_Stmnt :: T_Stmnt ->
              Inh_Stmnt ->
              Syn_Stmnt
wrap_Stmnt sem (Inh_Stmnt) =
    (let ( _lhsOself) = sem
     in  (Syn_Stmnt _lhsOself))
sem_Stmnt_Stmnt :: SourcePos ->
                   T_Stmnt_ ->
                   T_Stmnt
sem_Stmnt_Stmnt pos_ t_ =
    (let _lhsOself :: Stmnt
         _tIself :: Stmnt_
         _self =
             Stmnt pos_ _tIself
         _lhsOself =
             _self
         ( _tIself) =
             t_
     in  ( _lhsOself))
-- StmntL ------------------------------------------------------
type StmntL = [Stmnt]
-- cata
sem_StmntL :: StmntL ->
              T_StmntL
sem_StmntL list =
    (Prelude.foldr sem_StmntL_Cons sem_StmntL_Nil (Prelude.map sem_Stmnt list))
-- semantic domain
type T_StmntL = ( StmntL)
data Inh_StmntL = Inh_StmntL {}
data Syn_StmntL = Syn_StmntL {self_Syn_StmntL :: StmntL}
wrap_StmntL :: T_StmntL ->
               Inh_StmntL ->
               Syn_StmntL
wrap_StmntL sem (Inh_StmntL) =
    (let ( _lhsOself) = sem
     in  (Syn_StmntL _lhsOself))
sem_StmntL_Cons :: T_Stmnt ->
                   T_StmntL ->
                   T_StmntL
sem_StmntL_Cons hd_ tl_ =
    (let _lhsOself :: StmntL
         _hdIself :: Stmnt
         _tlIself :: StmntL
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_StmntL_Nil :: T_StmntL
sem_StmntL_Nil =
    (let _lhsOself :: StmntL
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Stmnt_ ------------------------------------------------------
data Stmnt_ = RootSet (StmntL)
            | StmntL (StmntL)
            | Assgn (Var) (IntExpr)
            | IfThenElse (BoolExpr) (Stmnt) (Stmnt)
            | While (BoolExpr) (Stmnt)
            | Skip
-- cata
sem_Stmnt_ :: Stmnt_ ->
              T_Stmnt_
sem_Stmnt_ (RootSet _ss) =
    (sem_Stmnt__RootSet (sem_StmntL _ss))
sem_Stmnt_ (StmntL _ss) =
    (sem_Stmnt__StmntL (sem_StmntL _ss))
sem_Stmnt_ (Assgn _x _ie) =
    (sem_Stmnt__Assgn _x _ie)
sem_Stmnt_ (IfThenElse _cond _thenBody _elseBody) =
    (sem_Stmnt__IfThenElse _cond (sem_Stmnt _thenBody) (sem_Stmnt _elseBody))
sem_Stmnt_ (While _cond _body) =
    (sem_Stmnt__While _cond (sem_Stmnt _body))
sem_Stmnt_ (Skip) =
    (sem_Stmnt__Skip)
-- semantic domain
type T_Stmnt_ = ( Stmnt_)
data Inh_Stmnt_ = Inh_Stmnt_ {}
data Syn_Stmnt_ = Syn_Stmnt_ {self_Syn_Stmnt_ :: Stmnt_}
wrap_Stmnt_ :: T_Stmnt_ ->
               Inh_Stmnt_ ->
               Syn_Stmnt_
wrap_Stmnt_ sem (Inh_Stmnt_) =
    (let ( _lhsOself) = sem
     in  (Syn_Stmnt_ _lhsOself))
sem_Stmnt__RootSet :: T_StmntL ->
                      T_Stmnt_
sem_Stmnt__RootSet ss_ =
    (let _lhsOself :: Stmnt_
         _ssIself :: StmntL
         _self =
             RootSet _ssIself
         _lhsOself =
             _self
         ( _ssIself) =
             ss_
     in  ( _lhsOself))
sem_Stmnt__StmntL :: T_StmntL ->
                     T_Stmnt_
sem_Stmnt__StmntL ss_ =
    (let _lhsOself :: Stmnt_
         _ssIself :: StmntL
         _self =
             StmntL _ssIself
         _lhsOself =
             _self
         ( _ssIself) =
             ss_
     in  ( _lhsOself))
sem_Stmnt__Assgn :: Var ->
                    IntExpr ->
                    T_Stmnt_
sem_Stmnt__Assgn x_ ie_ =
    (let _lhsOself :: Stmnt_
         _self =
             Assgn x_ ie_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Stmnt__IfThenElse :: BoolExpr ->
                         T_Stmnt ->
                         T_Stmnt ->
                         T_Stmnt_
sem_Stmnt__IfThenElse cond_ thenBody_ elseBody_ =
    (let _lhsOself :: Stmnt_
         _thenBodyIself :: Stmnt
         _elseBodyIself :: Stmnt
         _self =
             IfThenElse cond_ _thenBodyIself _elseBodyIself
         _lhsOself =
             _self
         ( _thenBodyIself) =
             thenBody_
         ( _elseBodyIself) =
             elseBody_
     in  ( _lhsOself))
sem_Stmnt__While :: BoolExpr ->
                    T_Stmnt ->
                    T_Stmnt_
sem_Stmnt__While cond_ body_ =
    (let _lhsOself :: Stmnt_
         _bodyIself :: Stmnt
         _self =
             While cond_ _bodyIself
         _lhsOself =
             _self
         ( _bodyIself) =
             body_
     in  ( _lhsOself))
sem_Stmnt__Skip :: T_Stmnt_
sem_Stmnt__Skip =
    (let _lhsOself :: Stmnt_
         _self =
             Skip
         _lhsOself =
             _self
     in  ( _lhsOself))