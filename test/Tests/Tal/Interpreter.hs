module Tests.Tal.Interpreter (interpreterTest) where

import Tal.Context
import Tal.Instruction
import Tal.Interpreter
import Test.Tasty
import Test.Tasty.HUnit

interpreterTest :: TestTree
interpreterTest = testGroup "Tal.Interpreter"
    [ testCase "mov rv, 1; halt [int]" $ do
        ctx <- createContext
        createLoadiInst ctx rv 1
        createHaltInst ctx
        ret <- runInterpreter ctx
        ret @?= (1 :: Int)
    {-, testCase "mov rv, 1; add rv, rv, 1; halt [int]" $ do
        ctx <- createContext
        createLoadiInst ctx rv 1
        createAddInst ctx rv rv 1
        ret <- runInterpreter ctx
        ret @?= (2 :: Int)
    , testCase "mov r1, 0; bnz r1, l1; mov rv, 10; halt [int]; \
        \l1: mov rv, -10; halt [int]" $ do
        ctx <- createContext
        createLoadiInst ctx r0 0
        createBneInst ctx r1 0 4
        createLoadiInst ctx rv 10
        createHaltInst ctx
        createLoadiInst ctx rv (-10)
        createHaltInst ctx
        ret <- runInterpreter ctx
        ret @?= (10 :: Int) -}
    ]
