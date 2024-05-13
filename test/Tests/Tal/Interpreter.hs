module Tests.Tal.Interpreter (interpreterTest) where

import Control.Lens.Cons
import Data.Map.Strict   qualified as M
import Data.Unique
import Tal.Constant
import Tal.Constructors
import Tal.Interpreter
import Tal.Syntax
import Test.Tasty
import Test.Tasty.HUnit

rv :: Reg
rv = RVReg

interpreterTest :: TestTree
interpreterTest = testGroup "Tal.Interpreter"
    [ testCase "mov rv, 1; halt [int]" $ do
        let instrs =
                [IMove rv (VWord (VInt 1))] <>| IHalt TInt
        ret <- runProgram (mkProgramFromInstrs instrs)
        ret @?= (1 :: Word)
    , testCase "mov rv, 1; add rv, rv, 1; halt [int]" $ do
        let instrs =
                [ IMove rv (VWord (VInt 1))
                , IAop Add rv rv (VWord (VInt 1))
                ] <>| IHalt TInt
        ret <- runProgram (mkProgramFromInstrs instrs)
        ret @?= (2 :: Word)
    , testCase "mov r1, 0; bnz r1, l1; mov rv, 10; halt [int]; \
        \l1: mov rv, -10; halt [int]" $ do
        l1 <- Name "l1" <$> newUnique
        let heaps = M.fromList [(l1, HCode [] M.empty SNil (IMove rv (VWord (VInt (-10))) <| IHalt TInt))]
            instrs =
                [ IMove Reg1 (VWord (VInt 0))
                , IBop Bnz Reg1 (VWord (VLabel l1))
                , IMove rv (VWord (VInt 10))
                ] <>| IHalt TInt
        ret <- runProgram (heaps, instrs)
        ret @?= (10 :: Word)
    ]
