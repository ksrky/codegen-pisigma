import Anf.Check                 qualified as Anf
import AnfClosure
import ClosUnty
import Closure.Check             qualified as Closure
import Lambda qualified
import Lambda.Check              qualified as Lambda
import LambdaAnf
import Parser
import RawLambda

import Control.Lens
import Data.ByteString.Lazy      qualified as BL
import Data.Map.Strict           qualified as Map
import Data.Text                 (Text)
import Data.Text.Encoding
import Prettyprinter             hiding (pretty)
import Prettyprinter.Prec
import Prettyprinter.Render.Text
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Tests.Anf                 qualified as Anf
import Tests.Closure             qualified as Closure
import Tests.Lambda              qualified as Lambda
import Tests.Raw                 qualified as Raw

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "codegen-pisigma" [parserTests, scopeTests, stepTests, goldenTests]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ testCase "42" $ do
      e <- parseProg "42"
      e @?= Raw.expMap Map.! "42"
  , testCase "x" $ do
      e <- parseProg "x"
      e @?= Raw.expMap Map.! "x"
  , testCase "f x" $ do
      e <- parseProg "f x"
      e @?= Raw.expMap Map.! "f x"
  , testCase "\\x -> x" $ do
      e <- parseProg "\\x -> x"
      e @?= Raw.expMap Map.! "\\x -> x"
  , testCase "let x = 42 in x" $ do
      e <- parseProg "let x = 42 in x"
      e @?= Raw.expMap Map.! "let x = 42 in x"
  , testCase "let x = 42 in x" $ do
      e <- parseProg "let rec f = \\x -> g x and g = \\x -> f x in f 0"
      e @?= Raw.expMap Map.! "let rec f = \\x -> g x and g = \\x -> f x in f 0"
  , testCase "True" $ do
      e <- parseProg "True"
      e @?= Raw.expMap Map.! "True"
  , testCase "if True then 1 else 0" $ do
      e <- parseProg "if True then 1 else 0"
      e @?= Raw.expMap Map.! "if True then 1 else 0"
  , testCase "2 * 3 == 6" $ do
      e <- parseProg "2 * 3 == 6"
      e @?= Raw.expMap Map.! "2 * 3 == 6"
  ]

scopeTests :: TestTree
scopeTests = testGroup "Scope tests"
  [ testCase "\\x -> x * x" $ do
      e1 <- parseProg "\\x -> x * x"
      e2 <- rawLambdaProgram e1
      case Lambda.stripAnnot (snd e2) of
        Lambda.ScopeTest1 x0 x1 x2 -> do x0 @?= x1; x1 @?= x2
        e                          -> assertFailure $ "unexpected: " ++ show e
  ]

stepTests :: TestTree
stepTests = testGroup "Step tests"
  [ testCaseSteps "42" $ \step -> do
      step "Parser"
      e1 <- parseProg "42"
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
      view _3 e4 @?= Closure.expMap Map.! "42"
      step "Closure.Check"
      Closure.checkProgram e4
      step "Done"
  , testCaseSteps "\\x -> x" $ \step -> do
      e1 <- parseProg "\\x -> x"
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
      e1 <- parseProg "let x = 42 in x"
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
  , testCaseSteps "(\\x -> x) 5" $ \step -> do
      e1 <- parseProg "(\\x -> x) 5"
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
  , testCaseSteps "2 * 3 == 6" $ \step -> do
      e1 <- parseProg "2 * 3 == 6"
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
      e1 <- parseProg "let quad = \\x -> let double = \\x -> x + x in double (double x) in quad 12"
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
  {- , testCaseSteps "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1" $ \step -> do
      e1 <- parseProg "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1"
      step "Lambda.Check"
      e2 <- rawLambdaProgram e1
      Lambda.checkProgram e2
      step "Anf.Check"
      let e3 = lambdaAnfProgram e2
      Anf.checkProgram e3
      step "Closure.Check"
      e4 <- anfClosureProgram e3
      Closure.checkProgram e4
      step "Done" -}
  ]

outputClosString :: Text -> IO BL.ByteString
outputClosString  inp = do
  raw_prog <- parseProg inp
  lam_prog <- rawLambdaProgram raw_prog
  let anf_prog = lambdaAnfProgram lam_prog
  clos_prog <- anfClosureProgram anf_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pretty clos_prog
  return $ BL.fromStrict $ encodeUtf8 out

outputUntyString :: Text -> IO BL.ByteString
outputUntyString  inp = do
  raw_prog <- parseProg inp
  lam_prog <- rawLambdaProgram raw_prog
  let anf_prog = lambdaAnfProgram lam_prog
  clos_prog <- anfClosureProgram anf_prog
  let unty_prog = c2uProg clos_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pretty unty_prog
  return $ BL.fromStrict $ encodeUtf8 out

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsString "\\x -> x" ".golden/clos_\\x -> x.txt" $ outputClosString "\\x -> x"
  , goldenVsString "(\\x -> x) 5" ".golden/clos_(\\x -> x) 5.txt" $ outputClosString "(\\x -> x) 5"
  , goldenVsString "\\x -> (\\y -> x + y)" ".golden/clos_\\x -> (\\y -> x + y).txt" $ outputClosString "\\x -> (\\y -> x + y)"
  , goldenVsString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5" ".golden/clos_let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5.txt" $ outputClosString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
  , goldenVsString "\\x -> x" ".golden/unty_\\x -> x.txt" $ outputUntyString "\\x -> x"
  , goldenVsString "(\\x -> x) 5" ".golden/unty_(\\x -> x) 5.txt" $ outputUntyString "(\\x -> x) 5"
  , goldenVsString "\\x -> (\\y -> x + y)" ".golden/unty_\\x -> (\\y -> x + y).txt" $ outputUntyString "\\x -> (\\y -> x + y)"
  , goldenVsString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5" ".golden/unty_let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5.txt" $ outputUntyString "let rec fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
  ]
