module Main where

import Types as T
import Check as Ch
import Prims as P

-- | Run all tests and print results.
main :: IO ()
main = do
    putStrLn "=== Base Kind Tests ===\n"
    testKTypeTInt
    testKTypeTBool
    testKTypeTUnit
    testKArrow
    testLambdaAnn
    testTRecCons
    putStrLn "\n=== Lambda Tests ===\n"
    testLambdaInt
    testLambdaBool
    testLambdaUnit
    testLambdaArrow
    testLambdaTRecCons
    putStrLn "\n=== All tests completed ==="

-- ---------------------------------------------------------------------------
-- Test infrastructure
-- ---------------------------------------------------------------------------

-- TInt
testKTypeTInt :: IO ()
testKTypeTInt = do
    putStrLn "Running testKTypeTInt..."
    let typeExpr = T.TBase T.TInt
    putStrLn $ "Test KType: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test KType: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testKTypeTBool :: IO ()
testKTypeTBool = do
    putStrLn "Running testKTypeTBool..."
    let typeExpr = T.TBase T.TBool
    putStrLn $ "Test KType: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test KType: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testKTypeTUnit :: IO ()
testKTypeTUnit = do
    putStrLn "Running testKTypeTUnit..."
    let typeExpr = T.TBase T.TUnit
    putStrLn $ "Test KType: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test KType: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testKArrow :: IO ()
testKArrow = do
    putStrLn "Running testKArrow..."
    let typeExpr = T.TBase (T.Arrow T.TInt T.TBool)
    putStrLn $ "Test KArrow: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test KArrow: " ++ show cstr
    putStrLn $ "Expected: CAnd []"


testLambdaAnn :: IO ()
testLambdaAnn = do
        putStrLn "Running testLambdaAnn..."
        let argKind = T.KBase T.BKType (T.Refinement ("v", P.typereft T.BEq (T.Pvar "v") (T.PTBaseTypes T.TUnit)))
            typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
        putStrLn $ "Test LambdaAnn: " ++ show typeExpr
        let cstr = Ch.vcgen typeExpr
        putStrLn $ "Test LambdaAnn: " ++ show cstr
        putStrLn $ "Expected: CAnd []"

testTRecCons :: IO ()
testTRecCons = do
    putStrLn "Running testTRecCons..."
    let typeExpr = T.TBase (T.TRecCons (T.TLabel "label") (T.TInt) (T.TRecNil))
    putStrLn $ "Test TRecCons: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test TRecCons: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

-- ---------------------------------------------------------------------------
-- Lambda tests for each base type
-- ---------------------------------------------------------------------------

testLambdaInt :: IO ()
testLambdaInt = do
    putStrLn "Running testLambdaInt..."
    let argKind = T.KBase T.BKType (T.Refinement ("v", T.PTBaseTypes T.TInt))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    putStrLn $ "Test LambdaInt: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test LambdaInt: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testLambdaBool :: IO ()
testLambdaBool = do
    putStrLn "Running testLambdaBool..."
    let argKind = T.KBase T.BKType (T.Refinement ("v", T.PTBaseTypes T.TBool))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    putStrLn $ "Test LambdaBool: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test LambdaBool: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testLambdaUnit :: IO ()
testLambdaUnit = do
    putStrLn "Running testLambdaUnit..."
    let argKind = T.KBase T.BKType (T.Refinement ("v", T.PTBaseTypes T.TUnit))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    putStrLn $ "Test LambdaUnit: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test LambdaUnit: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testLambdaArrow :: IO ()
testLambdaArrow = do
    putStrLn "Running testLambdaArrow..."
    let argKind = T.KBase T.BKType (T.Refinement ("v", T.PTBaseTypes (T.Arrow T.TInt T.TBool)))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    putStrLn $ "Test LambdaArrow: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test LambdaArrow: " ++ show cstr
    putStrLn $ "Expected: CAnd []"

testLambdaTRecCons :: IO ()
testLambdaTRecCons = do
    putStrLn "Running testLambdaTRecCons..."
    let argKind = T.KBase T.BKRec (T.Refinement ("v", T.PTBaseTypes (T.TRecCons (T.TLabel "label") T.TInt T.TRecNil)))
        typeExpr = T.TAnn (T.TLambda (T.LetBind "x") (T.TVar "x")) (T.KPi ("x", argKind) argKind)
    putStrLn $ "Test LambdaTRecCons: " ++ show typeExpr
    let cstr = Ch.vcgen typeExpr
    putStrLn $ "Test LambdaTRecCons: " ++ show cstr
    putStrLn $ "Expected: CAnd []"
