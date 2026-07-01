module Constraint
    ( Cstr(..)
    , Bind(..)
    , ctrue
    , runcvc4
    , dumpToSmt
    , smtString
    ) where

import Types as T
import System.Process (readProcess)

data Cstr
    = CPred T.Pred
    | CAnd [Cstr]
    | CAll Bind Cstr
    deriving (Show, Eq)

data Bind = Bind T.Identifier T.BaseKind T.Pred
    deriving (Show, Eq)

ctrue :: Cstr
ctrue = CAnd []


--- check satisfiabilty

runcvc4 :: FilePath -> IO String
runcvc4 path = do
    -- let cmd = "/home/afonso/aulas/semestre10/tese/repos/refK_artifact/cvc4 " ++ path
    output <- readProcess "/home/afonso/aulas/semestre10/tese/repos/refK_artifact/cvc4" ["--incremental", path] ""
    return output

dumpToSmt :: FilePath -> Constraint.Cstr -> IO ()
dumpToSmt path vc = writeFile path (smtString vc)

smtHeader :: String
smtHeader = unlines
  [ "DATATYPE Typ ="
  , "   nilrec  | cons(hlab:STRING, ht:Typ, tail:Typ)"
  , "  | FunType(dom:Typ, img:Typ) | Bool | Str"
  , "  | RefType(refof: Typ) | ColType(colof: Typ) | PolyType(map: Typ) | Unit | Num"
  , "END;"
  , "appTyp : (Typ,Typ) -> Typ;"
  , ""
  , "is_rec : Typ -> BOOLEAN;"
  , "lab : Typ -> SET OF STRING;"
  , "apartl : (SET OF STRING, SET OF STRING) -> BOOLEAN;"
  , ""
  , "ASSERT FORALL (r:Typ):"
  , "   is_rec(r) <=>  (is_nilrec(r) OR ( is_cons(r) AND is_rec(tail(r)) AND NOT(hlab(r) IS_IN lab(tail(r))))) ;"
  , ""
  , "ASSERT FORALL (r:Typ): "
  , "      ((is_rec(r) AND is_nilrec(r)) => (lab(r) = {} :: SET OF STRING)) AND"
  , "     ((is_rec(r) AND is_cons(r))  => ((lab(r) = {hlab(r)} | lab(tail(r))) )); "
  , ""
  , ""
  , "ASSERT FORALL (s1:SET OF STRING, s2:SET OF STRING): "
  , "(apartl (s1,s2) <=> FORALL (l:STRING): NOT ((l IS_IN s1) AND (l IS_IN s2)));"
  , ""
  , ""
  , "%apartl : (SET OF STRING, SET OF STRING) -> BOOLEAN;"
  , "ASSERT FORALL (s1:SET OF STRING, s2:SET OF STRING): "
  , "(apartl (s1,s2) <=> (s1 & s2 = {} :: SET OF STRING));"
  , ""
  , "%ASSERT FORALL (r1:Typ, r2:Typ): "
  , "%(is_rec(r1) AND is_rec(r2) AND apartl(lab(r1),lab(r2)) AND NOT (is_nilrec(r1))) "
  , "%=> apartl(lab(tail(r1)),lab(r2));"
  , "%ASSERT FORALL (r1:Typ, r2:Typ): "
  , "%(is_rec(r1) AND is_rec(r2) AND apartl(lab(r1),lab(r2)) AND NOT (is_nilrec(r2))) "
  , "%=> apartl(lab(r1),lab(tail(r2)));"
  , ""
  , ""
  , "%%% QUERIES BEGIN HERE"
  ]



-- make queries in the style of
{- PUSH;
QUERY FORALL (f : Typ):
(is_FunType(f) AND dom(f) = Num) => (dom(f) = Num);
POP;
 -}
-- using cvc4 native input language
gen_query :: Constraint.Cstr -> String
gen_query cstr =
  "PUSH;\n" ++
  "QUERY " ++ (constraintToCVC4 cstr) ++ ";\n" ++
  "POP;\n"

smtString :: Constraint.Cstr -> String
smtString cstr =
  smtHeader ++
  gen_query cstr  

constraintToCVC4 :: Constraint.Cstr -> String
constraintToCVC4 (Constraint.CPred p) = predToCVC4 p
constraintToCVC4 (Constraint.CAnd cs) =
  "(" ++ (concatMap (\c -> constraintToCVC4 c ++ " AND ") cs) ++ " TRUE)"
constraintToCVC4 (Constraint.CAll (Constraint.Bind v k p) c) =
  "FORALL (" ++ v ++ " : " ++ kindToCVC4 k ++ "): (" ++
  predToCVC4 p ++ " => " ++
  constraintToCVC4 c ++ ")"

predToCVC4 :: T.Pred -> String
predToCVC4 T.PTrue = "TRUE"
predToCVC4 T.PFalse = "FALSE"
predToCVC4 (T.PTBaseTypes b) = baseTypeToCVC4 b
predToCVC4 (T.PLab b) = "lab(" ++ baseTypeToCVC4 b ++ ")"
predToCVC4 (T.Pvar v) = v
predToCVC4 (T.PArrow p1 p2) = "(" ++ predToCVC4 p1 ++ " => " ++ predToCVC4 p2 ++ ")"
predToCVC4 (T.PInterp T.BApart p1 p2) =
  "apartl(" ++ predToCVC4 p1 ++ ", " ++ predToCVC4 p2 ++ ")"
predToCVC4 (T.PInterp op p1 p2) =
  "(" ++ predToCVC4 p1 ++ " " ++ typeOpToCVC4 op ++ " " ++ predToCVC4 p2 ++ ")"
predToCVC4 (T.PAnd p1 p2) = "(" ++ predToCVC4 p1 ++ " AND " ++ predToCVC4 p2 ++ ")"
predToCVC4 (T.POr p1 p2) = "(" ++ predToCVC4 p1 ++ " OR " ++ predToCVC4 p2 ++ ")"
predToCVC4 (T.PNot p) = "(NOT " ++ predToCVC4 p ++ ")"
-- predToCVC4 (T.PUniterp f ps) = "(" ++ f ++ " " ++ unwords (map predToCVC4 ps) ++ ")" 

baseTypeToCVC4 :: T.BaseTypes -> String
baseTypeToCVC4 T.TUnit = "Unit"
baseTypeToCVC4 T.TInt = "Num"
baseTypeToCVC4 T.TBool = "Bool"
baseTypeToCVC4 (T.Arrow t1 t2) = "FunType(" ++ baseTypeToCVC4 t1 ++ ", " ++ baseTypeToCVC4 t2 ++ ")"
baseTypeToCVC4 (T.TRecCons (T.TLabel l) t1 t2) = "cons(\"" ++ l ++ "\", " ++ baseTypeToCVC4 t1 ++ ", " ++ baseTypeToCVC4 t2 ++ ")"
baseTypeToCVC4 T.TRecNil = "nilrec"
baseTypeToCVC4 (T.TLabel l) = "\"" ++ l ++ "\""
baseTypeToCVC4 (T.PBIn _) = error "baseTypeToCVC4: PBIn not expected as a base type"
baseTypeToCVC4 (T.TRecCons l t1 t2) = "cons(" ++ show (T.baseTypetoIdentifier l) ++ ", " ++ baseTypeToCVC4 t1 ++ ", " ++ baseTypeToCVC4 t2 ++ ")"   -- non-label first field


kindToCVC4 :: T.BaseKind -> String
kindToCVC4 T.BKType = "Typ"
kindToCVC4 T.BKRec = "Typ"
kindToCVC4 T.BKFun = "Typ"
kindToCVC4 T.BKLabel = "STRING"

typeOpToCVC4 :: T.TypeOp -> String
typeOpToCVC4 T.BEq = "="
typeOpToCVC4 T.BAnd = "AND"
typeOpToCVC4 T.BOr = "OR"
typeOpToCVC4 T.BMember = "IS_IN"
typeOpToCVC4 T.BApart = "apartl"



