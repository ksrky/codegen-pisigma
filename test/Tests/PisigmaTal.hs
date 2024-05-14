module Tests.PisigmaTal (testsPisigmaTal) where

import Data.ByteString.Lazy      qualified as BL
import Data.Map.Strict           qualified as Map
import Data.Text                 (Text)
import Data.Text.Encoding
import PisigmaTal.Anf.Check      qualified as Anf
import PisigmaTal.AnfClosure
import PisigmaTal.Closure.Check  qualified as Closure
import PisigmaTal.ClosureTal
import PisigmaTal.Lambda         qualified as Lambda
import PisigmaTal.Lambda.Check   qualified as Lambda
import PisigmaTal.LambdaAnf
import PisigmaTal.Parser
import PisigmaTal.RawLambda
import Prettyprinter             hiding (pretty)
import Prettyprinter.Prec
import Prettyprinter.Render.Text
import Tal.Check                 qualified as Tal
import Tal.Prettyprint
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Tests.PisigmaTal.Anf      qualified as Anf
import Tests.PisigmaTal.Closure  qualified as Closure
import Tests.PisigmaTal.Lambda   qualified as Lambda
import Tests.PisigmaTal.Raw      qualified as Raw

testsPisigmaTal :: TestTree
testsPisigmaTal = testGroup "pisigma-tal" [parserTests, scopeTests, stepTests, goldenTests]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ testCase "42" $ do
      e <- parseProgram "42"
      e @?= Raw.expMap Map.! "42"
  , testCase "x" $ do
      e <- parseProgram "x"
      e @?= Raw.expMap Map.! "x"
  , testCase "f x" $ do
      e <- parseProgram "f x"
      e @?= Raw.expMap Map.! "f x"
  , testCase "\\x -> x" $ do
      e <- parseProgram "\\x -> x"
      e @?= Raw.expMap Map.! "\\x -> x"
  , testCase "let x = 42 in x" $ do
      e <- parseProgram "let x = 42 in x"
      e @?= Raw.expMap Map.! "let x = 42 in x"
  , testCase "let x = 42 in x" $ do
      e <- parseProgram "let rec f = \\x -> g x and g = \\x -> f x in f 0"
      e @?= Raw.expMap Map.! "let rec f = \\x -> g x and g = \\x -> f x in f 0"
  , testCase "True" $ do
      e <- parseProgram "True"
      e @?= Raw.expMap Map.! "True"
  , testCase "if True then 1 else 0" $ do
      e <- parseProgram "if True then 1 else 0"
      e @?= Raw.expMap Map.! "if True then 1 else 0"
  , testCase "2 * 3 == 6" $ do
      e <- parseProgram "2 * 3 == 6"
      e @?= Raw.expMap Map.! "2 * 3 == 6"
  ]

scopeTests :: TestTree
scopeTests = testGroup "Scope tests"
  [ testCase "\\x -> x * x" $ do
      e1 <- parseProgram "\\x -> x * x"
      e2 <- rawLambdaProgram e1
      case Lambda.stripAnnot (snd e2) of
        Lambda.ScopeTest1 x0 x1 x2 -> do x0 @?= x1; x1 @?= x2
        e                          -> assertFailure $ "unexpected: " ++ show e
  , testCase "\\x -> \\x -> x" $ do
      e1 <- parseProgram "\\x -> \\x -> x"
      e2 <- rawLambdaProgram e1
      case Lambda.stripAnnot (snd e2) of
        Lambda.ScopeTest2 x0 x1 x2 -> do x1 @?= x2; x0 /= x2 @? "x0 /= x2"
        e                          -> assertFailure $ "unexpected: " ++ show e
  , testCase "\\x -> \\y -> y" $ do
      e1 <- parseProgram "\\x -> \\x -> x"
      e2 <- rawLambdaProgram e1
      case Lambda.stripAnnot (snd e2) of
        Lambda.ScopeTest3 x y0 y1 -> do y0 @?= y1; x /= y0 @? "x /= y0"; x /= y1 @? "x /= y1"
        e                         -> assertFailure $ "unexpected: " ++ show e
  ]

stepTests :: TestTree
stepTests = testGroup "Step tests"
  [ testCaseSteps "42" $ \step -> do
      step "Parser"
      e1 <- parseProgram "42"
      e1 @?= Raw.expMap Map.! "42"
      step "RawLambda"
      e2 <- rawLambdaProgram e1
      snd e2 @?= Lambda.expMap Map.! "42"
      step "Lambda.Check"
      Lambda.checkProgram e2
      step " LambdaAnf"
      let e3 = lambdaAnfProgram e2
      snd e3 @?= Anf.expMap Map.! "42"
      step "Anf.Check"
      Anf.checkProgram e3
      step "AnfClosure"
      e4 <- anfClosureProgram e3
      snd (snd e4) @?= Closure.expMap Map.! "42"
      step "Closure.Check"
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "\\x -> x" $ \step -> do
      e1 <- parseProgram "\\x -> x"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "let x = 42 in x" $ \step -> do
      e1 <- parseProgram "let x = 42 in x"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Tal.Check"
      e5 <- closureTalProgram e4
      Tal.checkProgram e5
      step "Done"
  , testCaseSteps "(\\x -> x) 5" $ \step -> do
      e1 <- parseProgram "(\\x -> x) 5"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Tal.Check"
      e5 <- closureTalProgram e4
      Tal.checkProgram e5
      step "Done"
  , testCaseSteps "2 * 3 == 6" $ \step -> do
      e1 <- parseProgram "2 * 3 == 6"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "let quad = \\x -> let double = \\x -> x + x in double (double x) in quad 12" $ \step -> do
      e1 <- parseProgram "let quad = \\x -> let double = \\x -> x + x in double (double x) in quad 12"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1" $ \step -> do
      e1 <- parseProgram "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "let a = 10 in let rec f = \\x -> x + id a and id = \\x -> x in f 5" $ \step -> do
      e1 <- parseProgram "let a = 10 in let rec f = \\x -> x + id a and id = \\x -> x in f 5"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "let rec iseven = \\n -> if n == 0 then True else odd (n - 1)\
                    \and isodd = \\n -> if n == 0 then False else even (n - 1) in iseven 7" $ \step -> do
      e1 <- parseProgram "let rec even = \\n -> if n == 0 then True else odd (n - 1)\
                            \and odd = \\n -> if n == 0 then False else even (n - 1) in even 7"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden tests" [goldenClosTests, goldenTalTests]

outputClosString :: Text -> IO BL.ByteString
outputClosString  inp = do
  raw_prog <- parseProgram inp
  lam_prog <- rawLambdaProgram raw_prog
  let anf_prog = lambdaAnfProgram lam_prog
  clos_prog <- anfClosureProgram anf_prog
  let clos_texp = snd clos_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pretty clos_texp
  return $ BL.fromStrict $ encodeUtf8 out

goldenClosTests :: TestTree
goldenClosTests = testGroup "Golden Closure tests"
  [ goldenVsString "\\x -> x" ".golden/clos_\\x -> x.txt" $ outputClosString "\\x -> x"
  , goldenVsString "(\\x -> x) 5" ".golden/clos_(\\x -> x) 5.txt" $ outputClosString "(\\x -> x) 5"
  , goldenVsString "\\x -> (\\y -> x + y)" ".golden/clos_\\x -> (\\y -> x + y).txt" $ outputClosString "\\x -> (\\y -> x + y)"
  , goldenVsString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
    ".golden/clos_let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5.txt"
    $ outputClosString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
  , goldenVsString "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1"
    ".golden/clos_let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1.txt"
    $ outputClosString "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1"
  , goldenVsString "let a = 10 in let rec f = \\x -> x + id a and id = \\x -> x in f 5"
    ".golden/clos_let a = 10 in let rec f = \\x -> x + id a and id = \\x -> x in f 5.txt"
    $ outputClosString "let a = 10 in let rec f = \\x -> x + id a and id = \\x -> x in f 5"
  ]

outputTalString :: Text -> IO BL.ByteString
outputTalString  inp = do
  raw_prog <- parseProgram inp
  lambda_prog <- rawLambdaProgram raw_prog
  let anf_prog = lambdaAnfProgram lambda_prog
  closure_prog <- anfClosureProgram anf_prog
  tal_prog <- closureTalProgram closure_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pprtal tal_prog
  return $ BL.fromStrict $ encodeUtf8 out

goldenTalTests :: TestTree
goldenTalTests = testGroup "Golden TAL test"
  [ goldenVsString "42" ".golden/tal_42.txt" $ outputTalString "42"
  , goldenVsString "(\\x -> x) 5" ".golden/tal_(\\x -> x) 5.txt" $ outputTalString "(\\x -> x) 5"
  ]
