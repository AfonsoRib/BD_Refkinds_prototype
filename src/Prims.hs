module Prims where

import Types as T

constKind :: T.BaseTypes -> T.Rkind
constKind (T.TRecCons (T.TLabel l) t1 t2) = T.KBase T.BKRec (T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") (T.PTBaseTypes (T.TRecCons (T.TLabel l) t1 t2))))
constKind t = T.KBase T.BKType (T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") (T.PTBaseTypes t)))

--constKind (T.PBIn op) = T.KBase T.BKType (T.Refinement ("v", typereft op (T.Pvar "v") (T.PUniterp "PBIn" [T.Pvar "v"])))
constKind _ = error "constKind: unsupported base type"

{- 
exprReft :: PrimOp -> Pred -> Reft
exprReft _ e = Reft (vv_, PInterp BEq (PVar vv_) e)
    where vv_ = "v" -}


typereft :: T.TypeOp -> T.Pred -> T.Pred -> T.Pred
typereft _ v base = T.PInterp T.BEq v base