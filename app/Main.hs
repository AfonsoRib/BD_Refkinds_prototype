module Main where

import Types as T
import Check as Ch
import Prims as P
import Constraint as C
import System.IO (hFlush, stdout)

-- | Run all tests and print results.
main :: IO ()
main = do
    putStrLn "=== Base Kind Tests ===\n"
    checkConstraint "testKTypeTInt" testKTypeTInt
    checkConstraint "testKTypeTBool" testKTypeTBool
    checkConstraint "testKTypeTUnit" testKTypeTUnit
    checkConstraint "testKArrow" testKArrow
    checkConstraint "testLambdaAnn" testLambdaAnn
    checkConstraint "testTRecCons" testTRecCons
    putStrLn "\n=== Lambda Tests ===\n"
    checkConstraint "testLambdaInt" testLambdaInt
    checkConstraint "testLambdaBool" testLambdaBool
    checkConstraint "testLambdaUnit" testLambdaUnit
    checkConstraint "testLambdaArrow" testLambdaArrow
    checkConstraint "testLambdaTRecCons" testLambdaTRecCons
    checkConstraint "testIntAnn" testIntAnn
    putStrLn "\n=== Record BMember / BApart Tests ===\n"
    checkConstraint "testRecBMember" testRecBMember
    checkConstraint "testLambdaBMember" testLambdaBMember
    checkConstraint "testRecBApart" testRecBApart
    checkConstraint "testRecNotApart" testRecNotApart
    checkConstraint "testRecMultiBMember" testRecMultiBMember
    checkConstraint "testRecMultiBApart" testRecMultiBApart
    putStrLn "\n=== PBIn Tests ===\n"
    putStrLn "\n--- PBIn applied to concrete types ---\n"
    checkConstraint "testPBInBEqApplied" testPBInBEqApplied
    checkConstraint "testPBInBEqAppliedDiff" testPBInBEqAppliedDiff
    putStrLn "\n--- Lambda body using PBIn ---\n"
    checkConstraint "testLambdaPBInBody" testLambdaPBInBody
    putStrLn "\n--- Outside variables in refinement (using PBIn BEq) ---\n"
    checkConstraint "testLambdaPBInRefOuter" testLambdaPBInRefOuter
    putStrLn "\n=== Subkind Tests ===\n"
    putStrLn "\n--- BKRec / BKFun <: BKType ---\n"
    checkConstraint "testRecAsBKType" testRecAsBKType
    checkConstraint "testArrowAsBKType" testArrowAsBKType
    checkConstraint "testRecRefinedAsBKType" testRecRefinedAsBKType
    checkConstraint "testLambdaRecAsBKType" testLambdaRecAsBKType
    putStrLn "\n=== All tests completed ==="

-- ---------------------------------------------------------------------------
-- SMT validity checking helper
-- ---------------------------------------------------------------------------

checkConstraint :: String -> IO C.Cstr -> IO ()
checkConstraint name test = do
    putStr $ "  " ++ name ++ "... "
    hFlush stdout
    cstr <- test
    let smtFile = name ++ ".cvc4"
    C.dumpToSmt smtFile cstr
    output <- C.runcvc4 smtFile
    putStrLn $ "CVC4 output: " ++ take 80 output ++ "..."
    {- let isSat = "INVALID" `elem` words output
        isValid = "VALID" `elem` words output
    if isValid
        then putStrLn "VALID"
        else if isSat
            then putStrLn "INVALID (counterexample found)"
            else putStrLn $ take 80 output ++ "..."
 -}
-- ---------------------------------------------------------------------------
-- Test generators (each returns the constraint for SMT checking)
-- ---------------------------------------------------------------------------

-- TInt
testKTypeTInt :: IO C.Cstr
testKTypeTInt = do
    let typeExpr = T.TBase T.TInt
    pure $ Ch.vcgen typeExpr

testKTypeTBool :: IO C.Cstr
testKTypeTBool = do
    let typeExpr = T.TBase T.TBool
    pure $ Ch.vcgen typeExpr

testKTypeTUnit :: IO C.Cstr
testKTypeTUnit = do
    let typeExpr = T.TBase T.TUnit
    pure $ Ch.vcgen typeExpr

testKArrow :: IO C.Cstr
testKArrow = do
    let typeExpr = T.TBase (T.Arrow T.TInt T.TBool)
    pure $ Ch.vcgen typeExpr


testLambdaAnn :: IO C.Cstr
testLambdaAnn = do
        let argKind = P.constKind T.TUnit
            typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
        pure $ Ch.vcgen typeExpr

testTRecCons :: IO C.Cstr
testTRecCons = do
    let typeExpr = T.TBase (T.TRecCons (T.TLabel "label") (T.TInt) (T.TRecNil))
    pure $ Ch.vcgen typeExpr

-- ---------------------------------------------------------------------------
-- Lambda tests for each base type
-- ---------------------------------------------------------------------------

testLambdaInt :: IO C.Cstr
testLambdaInt = do
    let argKind = P.constKind T.TInt
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    pure $ Ch.vcgen typeExpr

testLambdaBool :: IO C.Cstr
testLambdaBool = do
    let argKind = P.constKind T.TBool
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    pure $ Ch.vcgen typeExpr

testLambdaUnit :: IO C.Cstr
testLambdaUnit = do
    let argKind = P.constKind T.TUnit
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    pure $ Ch.vcgen typeExpr

testLambdaArrow :: IO C.Cstr
testLambdaArrow = do
    let argKind = P.constKind (T.Arrow T.TInt T.TBool)
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    pure $ Ch.vcgen typeExpr

testLambdaTRecCons :: IO C.Cstr
testLambdaTRecCons = do
    let argKind = P.constKind (T.TRecCons (T.TLabel "label") T.TInt T.TRecNil)
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    pure $ Ch.vcgen typeExpr

--- test Ann

testIntAnn :: IO C.Cstr
testIntAnn = do
    let typeExpr = T.TAnn (T.TBase T.TInt) (P.constKind T.TInt)
    pure $ Ch.vcgen typeExpr

-- ---------------------------------------------------------------------------
-- BMember / BApart tests via type expressions (through vcgen)
-- ---------------------------------------------------------------------------

-- | Annotate {x: Int} with a kind asserting "x" is a label in it.
--   vcgen produces:
--     FORALL (v : Typ): (v = cons("x", Num, nilrec) => "x" IS_IN lab(cons("x", Num, nilrec)))
testRecBMember :: IO C.Cstr
testRecBMember = do
    let rec = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        kind = T.KBase T.BKRec (T.Refinement ("v",
            T.PInterp T.BMember (T.PTBaseTypes (T.TLabel "x")) (T.PLab rec)))
        typeExpr = T.TAnn (T.TBase rec) kind
    pure $ Ch.vcgen typeExpr

-- | Lambda λr: {x: Int | "x" ∈ lab(r)}. r
--   The argument kind refines the record binder with a BMember predicate.
testLambdaBMember :: IO C.Cstr
testLambdaBMember = do
    let rec = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        argKind = T.KBase T.BKRec (T.Refinement ("v",
            T.PInterp T.BMember (T.PTBaseTypes (T.TLabel "x")) (T.PLab rec)))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "r") (T.TVar "r"))
                    (T.KPi ("r", argKind) argKind)
    pure $ Ch.vcgen typeExpr

-- | Annotate {x: Int} with a kind asserting it is apart from {y: Bool}.
--   vcgen produces:
--     FORALL (v : Typ): (v = cons("x", Num, nilrec) =>
--       apartl(lab(cons("x", Num, nilrec)), lab(cons("y", Bool, nilrec))))
testRecBApart :: IO C.Cstr
testRecBApart = do
    let rec1 = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        rec2 = T.TRecCons (T.TLabel "y") T.TBool T.TRecNil
        kind = T.KBase T.BKRec (T.Refinement ("v",
            T.PInterp T.BApart (T.PLab rec1) (T.PLab rec2)))
        typeExpr = T.TAnn (T.TBase rec1) kind
    pure $ Ch.vcgen typeExpr

-- | Annotate {x: Int} with a kind asserting it is NOT apart from {x: Bool}.
--   vcgen produces:
--     FORALL (v : Typ): (v = cons("x", Num, nilrec) =>
--       NOT apartl(lab(cons("x", Num, nilrec)), lab(cons("x", Bool, nilrec))))
testRecNotApart :: IO C.Cstr
testRecNotApart = do
    let rec1 = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        rec2 = T.TRecCons (T.TLabel "x") T.TBool T.TRecNil
        kind = T.KBase T.BKRec (T.Refinement ("v",
            T.PNot (T.PInterp T.BApart (T.PLab rec1) (T.PLab rec2))))
        typeExpr = T.TAnn (T.TBase rec1) kind
    pure $ Ch.vcgen typeExpr

-- | Annotate {x: Int, y: Bool} with a kind asserting both labels are members.
--   vcgen produces:
--     FORALL (v : Typ): (v = cons("x", Num, cons("y", Bool, nilrec)) =>
--       ("x" IS_IN lab(...) AND "y" IS_IN lab(...) AND TRUE))
testRecMultiBMember :: IO C.Cstr
testRecMultiBMember = do
    let rec = T.TRecCons (T.TLabel "x") T.TInt
                (T.TRecCons (T.TLabel "y") T.TBool T.TRecNil)
        p1 = T.PInterp T.BMember (T.PTBaseTypes (T.TLabel "x")) (T.PLab rec)
        p2 = T.PInterp T.BMember (T.PTBaseTypes (T.TLabel "y")) (T.PLab rec)
        kind = T.KBase T.BKRec (T.Refinement ("v",
            T.PAnd p1 p2))
        typeExpr = T.TAnn (T.TBase rec) kind
    pure $ Ch.vcgen typeExpr

-- | Annotate {x: Int, y: Bool} with a kind asserting it is apart from {z: Unit}.
--   vcgen produces:
--     FORALL (v : Typ): (v = cons("x", Num, cons("y", Bool, nilrec)) =>
--       apartl(lab(cons("x", Num, cons("y", Bool, nilrec))),
--              lab(cons("z", Unit, nilrec))))
testRecMultiBApart :: IO C.Cstr
testRecMultiBApart = do
    let rec1 = T.TRecCons (T.TLabel "x") T.TInt
                (T.TRecCons (T.TLabel "y") T.TBool T.TRecNil)
        rec2 = T.TRecCons (T.TLabel "z") T.TUnit T.TRecNil
        kind = T.KBase T.BKRec (T.Refinement ("v",
            T.PInterp T.BApart (T.PLab rec1) (T.PLab rec2)))
        typeExpr = T.TAnn (T.TBase rec1) kind
    pure $ Ch.vcgen typeExpr


-- ---------------------------------------------------------------------------
-- Subkind tests — BKRec, BKFun, BKLabel are subkinds of BKType
-- ---------------------------------------------------------------------------

-- | A record type {x: Int} annotated with KBase BKType (True).
--   Tests BKRec <: BKType — a record kind should be accepted where
--   BKType is expected.
testRecAsBKType :: IO C.Cstr
testRecAsBKType = do
    let rec      = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        typeKind = P.bTrue T.BKType   -- {v:Typ | True}
        typeExpr = T.TAnn (T.TBase rec) typeKind
    pure $ Ch.vcgen typeExpr

-- | An arrow type Int → Bool annotated with KBase BKType (True).
--   Tests BKFun <: BKType — a function kind should be accepted where
--   BKType is expected.
testArrowAsBKType :: IO C.Cstr
testArrowAsBKType = do
    let arrow    = T.Arrow T.TInt T.TBool
        typeKind = P.bTrue T.BKType
        typeExpr = T.TAnn (T.TBase arrow) typeKind
    pure $ Ch.vcgen typeExpr

-- | A record type {x: Int} annotated with KBase BKType with a refinement
--   predicate. Tests that the refinement in the BKType target is properly
--   checked against the synthesized BKRec refinement.
testRecRefinedAsBKType :: IO C.Cstr
testRecRefinedAsBKType = do
    let rec      = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        -- target kind: {v:Typ | v = {x: Int}}   (BKType with a refinement)
        targetKind = T.KBase T.BKType (T.Refinement ("v",
            T.PInterp T.BEq (T.Pvar "v") (T.PTBaseTypes rec)))
        typeExpr = T.TAnn (T.TBase rec) targetKind
    pure $ Ch.vcgen typeExpr

-- | Lambda λr:{x:Int}. r where the binder's kind is BKRec but the
--   whole thing is checked against a BKType result.
--   This exercises BKRec <: BKType inside a Pi-kind context.
testLambdaRecAsBKType :: IO C.Cstr
testLambdaRecAsBKType = do
    let rec      = T.TRecCons (T.TLabel "x") T.TInt T.TRecNil
        argKind  = P.constKind rec     -- KBase BKRec {v | v = {x:Int}}
        resKind  = P.bTrue T.BKType    -- KBase BKType {v | True}
        lam = T.TAnn (T.TLambda (T.LetBind "r") (T.TVar "r"))
                (T.KPi ("r", argKind) resKind)
    pure $ Ch.vcgen lam

-- ---------------------------------------------------------------------------
-- PBIn tests — binary type operations in refinements
-- ---------------------------------------------------------------------------

-- | Apply PBIn BEq to TInt and TInt, producing a type whose refinement
--   says the two arguments are equal (int = int, trivially true).
--
--   This tests that constKind dispatches correctly to binOpKind for PBIn,
--   and that application substitutes the Pi-kind binders into the refinement.
testPBInBEqApplied :: IO C.Cstr
testPBInBEqApplied = do
    let pbin    = T.TBase (T.PBIn T.BEq)
        applied = T.TApp (T.TApp pbin (T.TBase T.TInt)) (T.TBase T.TInt)
        resultKind = P.bTrue T.BKType
        typeExpr   = T.TAnn applied resultKind
    pure $ Ch.vcgen typeExpr

-- | Apply PBIn BEq to TInt and TBool (different types).
--   The refinement will say int = bool, which is false — but the constraint
--   should still be well-formed. Tests that substitution works across
--   different base types via PBIn.
testPBInBEqAppliedDiff :: IO C.Cstr
testPBInBEqAppliedDiff = do
    let pbin    = T.TBase (T.PBIn T.BEq)
        applied = T.TApp (T.TApp pbin (T.TBase T.TInt)) (T.TBase T.TBool)
        resultKind = P.bTrue T.BKType
        typeExpr   = T.TAnn applied resultKind
    pure $ Ch.vcgen typeExpr

-- | Lambda where the *result refinement* references the outer binder using
--   PBIn BEq. Tests "outside variables inside the refinement using PBIn".
--
--   λx:Int. λy:Int. y   with result kind: {v:Type | v = x}
--
--   The refinement predicate PInterp BEq (Pvar "v") (Pvar "x") uses the
--   PBIn-linked operator BEq and references "x" — a variable bound by the
--   outer lambda. The implicationConstraint mechanism introduces a FORALL
--   for each binder, so "x" should be in scope.
testLambdaPBInRefOuter :: IO C.Cstr
testLambdaPBInRefOuter = do
    let intKind = P.constKind T.TInt
        -- result refinement: v = x   (using PBIn BEq operator)
        resRef  = T.Refinement ("v", T.PInterp T.BEq (T.Pvar "v") (T.Pvar "x"))
        resKind = T.KBase T.BKType resRef
        -- inner: λy:Int. y   with result kind {v:Type | v = x}
        inner   = T.TAnn (T.TLambda (T.LetBind "y") (T.TVar "y"))
                    (T.KPi ("y", intKind) resKind)
        -- outer: λx:Int. inner
        outer   = T.TAnn (T.TLambda (T.LetBind "x") inner)
                    (T.KPi ("x", intKind) (T.KPi ("y", intKind) resKind))
    pure $ Ch.vcgen outer



-- | Lambda whose body is a PBIn BEq application inside a lambda binder.
--
--   λx:Int. (PBIn BEq) Int Int
--
--   Tests that a fully-applied PBIn term works as the body of a lambda,
--   where the lambda binder "x" is in scope (though not referenced in this
--   particular refinement — the refinement after substitution says int = int).
testLambdaPBInBody :: IO C.Cstr
testLambdaPBInBody = do
    let intKind = P.constKind T.TInt
        pbin    = T.TBase (T.PBIn T.BEq)
        body    = T.TApp (T.TApp pbin (T.TBase T.TInt)) (T.TBase T.TInt)
        resKind = P.bTrue T.BKType
        -- λx:Int. (PBIn BEq Int Int)  with result kind {v:Typ | True}
        lam     = T.TAnn (T.TLambda (T.LetBind "x") body)
                    (T.KPi ("x", intKind) resKind)
    pure $ Ch.vcgen lam