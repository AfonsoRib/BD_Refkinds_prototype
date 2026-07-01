module Types where

type Identifier = String

-- to do inclusion φ ⊃ ψ


{- data PredType =  -}


data Pred = PTrue
            | PFalse
            | Pvar Identifier
            | PInterp TypeOp Pred Pred
            | PTBaseTypes BaseTypes -- TODO change to types later?
            | PLab BaseTypes        -- lab() for record label sets
            | PArrow Pred Pred
            | PAnd Pred Pred
            | POr Pred Pred
            | PNot Pred
--            | PUniterp Identifier [Pred] -- ???
             deriving (Eq, Show)

data BaseKind = BKType | BKRec | BKFun | BKLabel
  deriving (Eq, Show)

data Kind r 
  = KBase BaseKind r
  | KPi (Identifier, Kind r) (Kind r)
  deriving (Show, Eq)

newtype Refinement = Refinement (Identifier, Pred)
  deriving (Eq, Show)

type Rkind = Kind Refinement

--prims?
data BaseTypes = TUnit 
                | TInt 
                | TBool 
                | Arrow BaseTypes BaseTypes 
                | TRecCons BaseTypes BaseTypes BaseTypes -- label / type / treccons or TRecNil
                | TRecNil
                | TLabel Identifier
                | PBIn TypeOp
  deriving (Eq, Show)

data TypeOp = BEq | BAnd | BOr | BMember | BApart
    deriving (Eq, Ord, Show)

data LetBind = LetBind !Identifier deriving (Eq, Show)
data Type
  = TBase BaseTypes
    | TVar Identifier
    | TLambda LetBind Type
    | TApp Type Type
    | TAnn Type Rkind
  deriving (Eq, Show)


type Env x = [(Identifier, x)]

lookupEnv :: Identifier -> Env x -> Maybe x
lookupEnv _ [] = Nothing
lookupEnv x ((y, v):ys)
  | x == y    = Just v
  | otherwise = lookupEnv x ys


substPred :: Pred -> Identifier -> Identifier -> Pred
substPred PTrue _ _ = PTrue
substPred PFalse _ _ = PFalse
substPred (PTBaseTypes b) _ _ = PTBaseTypes b
substPred (Pvar v) x z
    | v == x    = Pvar z
    | otherwise = Pvar v
substPred (PArrow p1 p2) x z = PArrow (substPred p1 x z) (substPred p2 x z)
substPred (PInterp op p1 p2) x z = PInterp op (substPred p1 x z) (substPred p2 x z)
substPred (PAnd p1 p2) x z = PAnd (substPred p1 x z) (substPred p2 x z)
substPred (POr p1 p2) x z = POr (substPred p1 x z) (substPred p2 x z)
substPred (PNot p) x z = PNot (substPred p x z)
substPred (PLab b) _ _ = PLab b
-- substPred (PUniterp f ps) x z = PUniterp f (map (\p -> substPred p x z) ps)

substKind :: Rkind -> Identifier -> Identifier -> Rkind
substKind (KBase b (Refinement (v, p))) x z 
    | v == x    = KBase b (Refinement (v, p))
    | otherwise = KBase b (Refinement (v, substPred p x z))
substKind (KPi (v, k1) k2) x z 
    | v == x    = KPi (v, substKind k1 x z) k2
    | otherwise = KPi (v, substKind k1 x z) (substKind k2 x z)

-- TODO check if this is correct. slop

-- | Substitute a Type for an Identifier in a Pred, converting concrete
--   base types to PTBaseTypes so the CVC4 output is correct.
substPredType :: Pred -> Identifier -> Type -> Pred
substPredType PTrue _ _ = PTrue
substPredType PFalse _ _ = PFalse
substPredType (PTBaseTypes b) _ _ = PTBaseTypes b
substPredType (PLab b) _ _ = PLab b
substPredType (Pvar v) x (TBase bt)
    | v == x    = PTBaseTypes bt
    | otherwise = Pvar v
substPredType (Pvar v) _ _ = Pvar v   -- non-TBase (e.g. TVar) stays as-is
substPredType (PArrow p1 p2) x t = PArrow (substPredType p1 x t) (substPredType p2 x t)
substPredType (PInterp op p1 p2) x t = PInterp op (substPredType p1 x t) (substPredType p2 x t)
substPredType (PAnd p1 p2) x t = PAnd (substPredType p1 x t) (substPredType p2 x t)
substPredType (POr p1 p2) x t = POr (substPredType p1 x t) (substPredType p2 x t)
substPredType (PNot p) x t = PNot (substPredType p x t)

substKindType :: Rkind -> Identifier -> Type -> Rkind
substKindType (KBase b (Refinement (v, p))) x t 
    | v == x    = KBase b (Refinement (v, p))
    | otherwise = KBase b (Refinement (v, substPredType p x t))
substKindType (KPi (v, k1) k2) x t
    | v == x    = KPi (v, substKindType k1 x t) k2
    | otherwise = KPi (v, substKindType k1 x t) (substKindType k2 x t)


{- exprToSymbol :: Expr -> Symbol
exprToSymbol (Evar v) = v
exprToSymbol (ECon (PrimInt n)) = show n
exprToSymbol (ECon (PrimBool b)) = show b
exprToSymbol e = error ("exprToSymbol: cannot convert expression to symbol. " ++ show e)
 -}

{- typeToPred :: Type -> Pred
typeToPred (TBase TUnit) = PTUnit
typeToPred (TBase TInt) = PTInt
typeToPred (TBase TBool) = PTBool
typeToPred (TBase (Arrow t1 t2)) = PArrow (typeToPred t1) (typeToPred t2)
typeToPred _ = error "typeToPred: cannot convert type to predicate"
 -}

typeToIdentifier :: Type -> Identifier
typeToIdentifier (TBase (Arrow t1 t2)) = "(" ++ baseTypetoIdentifier t1 ++ " -> " ++ baseTypetoIdentifier t2 ++ ")"
typeToIdentifier (TBase x) = baseTypetoIdentifier x
typeToIdentifier _ = error "typeToIdentifier: cannot convert type to identifier"

baseTypetoIdentifier:: BaseTypes -> Identifier
baseTypetoIdentifier (TUnit) = "unit"
baseTypetoIdentifier (TInt) = "int"
baseTypetoIdentifier (TBool) = "bool"
baseTypetoIdentifier (Arrow t1 t2) = "(" ++ baseTypetoIdentifier t1 ++ " -> " ++ baseTypetoIdentifier t2 ++ ")"
baseTypetoIdentifier (TRecCons (TLabel l) t1 t2) = "{ " ++ l ++ " : " ++ baseTypetoIdentifier t1 ++ " | " ++ baseTypetoIdentifier t2 ++ " }"
baseTypetoIdentifier TRecNil = "{}"
baseTypetoIdentifier (TLabel l) = l
baseTypetoIdentifier (PBIn op) = show op
baseTypetoIdentifier (TRecCons l t1 t2) = "{ " ++ baseTypetoIdentifier l ++ " : " ++ baseTypetoIdentifier t1 ++ " | " ++ baseTypetoIdentifier t2 ++ " }"   -- non-label first field
