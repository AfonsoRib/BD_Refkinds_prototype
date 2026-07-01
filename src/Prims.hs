module Prims where

import Types as T
import qualified Data.Map as Map

constKind :: T.BaseTypes -> T.Rkind
constKind (T.TRecCons (T.TLabel l) t1 t2) = T.KBase T.BKRec (T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") (T.PType (T.TBase (T.TRecCons (T.TLabel l) t1 t2)))))
constKind (T.PBIn op) = binOpKind op
constKind t = T.KBase T.BKType (T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") (T.PType (T.TBase t))))
--constKind (T.PBIn op) = T.KBase T.BKType (T.Refinement ("v", typereft op (T.Pvar "v") (T.PUniterp "PBIn" [T.Pvar "v"])))
--constKind _ = error "constKind: unsupported base type"

binOpKind :: T.TypeOp -> T.Rkind
binOpKind op = case Map.lookup op binOpEnv of
    Just k -> k
    Nothing -> error $ "binOpKind: unsupported type operator: " ++ show op
{- 
exprReft :: PrimOp -> Pred -> Reft
exprReft _ e = Reft (vv_, PInterp BEq (PVar vv_) e)
    where vv_ = "v" -}


typereft :: T.TypeOp -> T.Pred -> T.Refinement
typereft _ t = T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") t)


bTrue :: T.BaseKind -> T.Rkind
bTrue k = T.KBase k (T.Refinement ("v", T.PTrue))


-- | Build a result refinement for a PBIn operation.
--   Unlike typereft, this does NOT wrap the predicate in another
--   PInterp BEq (Pvar "v") — doing so would produce invalid CVC4
--   like (v = (Num = Num)) since the inner predicate is Boolean
--   while v is Typ.
pbinReft :: T.Pred -> T.Refinement
pbinReft p = T.Refinement ("v", p)

binOpEnv :: Map.Map T.TypeOp T.Rkind
binOpEnv = Map.fromList [
    (T.BEq, T.KPi ("x", (bTrue T.BKType)) (T.KPi ("y", (bTrue T.BKType)) (T.KBase T.BKType (pbinReft (T.PInterp T.BEq (T.Pvar "x") (T.Pvar "y")))))),
    (T.BAnd, T.KPi ("x", (bTrue T.BKType)) (T.KPi ("y", (bTrue T.BKType)) (T.KBase T.BKType (pbinReft (T.PInterp T.BAnd (T.Pvar "x") (T.Pvar "y")))))),
    (T.BOr, T.KPi ("x", (bTrue T.BKType)) (T.KPi ("y", (bTrue T.BKType)) (T.KBase T.BKType (pbinReft (T.PInterp T.BOr (T.Pvar "x") (T.Pvar "y")))))),
    (T.BMember, T.KPi ("x", (bTrue T.BKType)) (T.KPi ("y", (bTrue T.BKType)) (T.KBase T.BKType (pbinReft (T.PInterp T.BMember (T.Pvar "x") (T.Pvar "y")))))),
    (T.BApart, T.KPi ("x", (bTrue T.BKType)) (T.KPi ("y", (bTrue T.BKType)) (T.KBase T.BKType (pbinReft (T.PInterp T.BApart (T.Pvar "x") (T.Pvar "y"))))))
    
    ]