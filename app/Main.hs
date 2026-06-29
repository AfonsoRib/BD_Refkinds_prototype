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
