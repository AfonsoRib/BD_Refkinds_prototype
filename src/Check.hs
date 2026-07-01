module Check where

import Types as T
import Constraint as C
import Prims as P

import Debug.Trace (trace)

vcgen :: T.Type -> C.Cstr
vcgen e =
    let (c, k) = synth [] e
    in trace ("VCGEN: constraint: " ++ show c ++ ", kind: " ++ show k) c 

synth :: T.Env T.Rkind -> T.Type -> (C.Cstr, T.Rkind)
synth _ (T.TBase t) = (C.ctrue, P.constKind t)
synth env (T.TVar x) =
    case T.lookupEnv x env of
        Just k -> (C.ctrue, k)
        Nothing -> error $ "synth: unbound variable: " ++ x

synth env (T.TApp e y) = case synth env e of
    (c, T.KPi (x, s) k) -> (C.CAnd [c, c'], substKindType k x y)
        where c' = check env y s
    (_, k) -> error $ "synth: TApp expected a Pi-kind, got: " ++ show k

synth env (T.TAnn t k) = (c, k)
    where c = check env t k

synth _ e = error $ "synth: not implemented for: " ++ show e


check :: T.Env T.Rkind -> T.Type -> T.Rkind -> C.Cstr

check env (T.TLambda (T.LetBind x) e) (T.KPi (x', kArg) kRes) = 
    trace ("check: lambda abstraction: " ++ show (T.TLambda (T.LetBind x) e) ++ " against kind: " ++ show (T.KPi (x', kArg) kRes)) $
    implicationConstraint x kArg c
    where
        env' = (x, kArg) : env
        c = check env' e kRes

{- check env e t = CAnd [c1, c2]
    where
        (c1, t') = synth env e
        c2 = sub t' t
         -}

check env t k = 
    trace ("check: type: " ++ show t ++ " against kind: " ++ show k) $
    C.CAnd [c1, c2]
    where
        (c1, k') = synth env t
        c2 = sub k' k

implicationConstraint :: T.Identifier -> T.Rkind -> C.Cstr -> C.Cstr
implicationConstraint x k c = case k of
    T.KBase b (T.Refinement (v, p)) -> C.CAll (C.Bind x b (T.substPred p v x)) c
    _ -> c
        
    

-- | Subkinding: BKRec, BKFun, BKLabel are subkinds of BKType.
--   Any value of a more specific base kind can be used where BKType is expected.
sub :: T.Rkind -> T.Rkind -> C.Cstr
sub (T.KBase b1 (T.Refinement (v1, p1))) (T.KBase b2 (T.Refinement (v2, p2)))
    | b1 == b2 || b2 == T.BKType = C.CAll (C.Bind v1 b1 p1) (C.CPred (T.substPred p2 v2 v1))
    | otherwise = error "sub: base kinds do not match"
sub (T.KPi (x1, kArg1) kRes1) (T.KPi (x2, kArg2) kRes2) = C.CAnd [sub kArg2 kArg1, sub kRes1 kRes2']
    where
        kRes2' = T.substKind kRes2 x2 x1
sub k1 k2 = error $ "sub: kinds do not match: " ++ show k1 ++ " and " ++ show k2