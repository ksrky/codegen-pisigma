import Anf.Tc                    qualified as Anf
import AnfClos
import ClosUnty
import Closure.Tc                qualified as Closure
import Data.ByteString.Lazy      qualified as BL
import Data.Map.Strict           qualified as Map
import Data.Text                 (Text)
import Data.Text.Encoding
import LamAnf
import Lambda                    qualified as L
import Lambda.Tc                 qualified as Lambda
import Parser
import Prettyprinter             hiding (pretty)
import Prettyprinter.Prec
import Prettyprinter.Render.Text
import RawLam
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
      e @?= Raw.progMap Map.! "42"
  , testCase "x" $ do
      e <- parseProg "x"
      e @?= Raw.progMap Map.! "x"
  , testCase "f x" $ do
      e <- parseProg "f x"
      e @?= Raw.progMap Map.! "f x"
  , testCase "\\x -> x" $ do
      e <- parseProg "\\x -> x"
      e @?= Raw.progMap Map.! "\\x -> x"
  , testCase "let x = 42 in x" $ do
      e <- parseProg "let x = 42 in x"
      e @?= Raw.progMap Map.! "let x = 42 in x"
  , testCase "let x = 42 in x" $ do
      e <- parseProg "let rec f = \\x -> g x and g = \\x -> f x in f 0"
      e @?= Raw.progMap Map.! "let rec f = \\x -> g x and g = \\x -> f x in f 0"
  , testCase "True" $ do
      e <- parseProg "True"
      e @?= Raw.progMap Map.! "True"
  , testCase "if True then 1 else 0" $ do
      e <- parseProg "if True then 1 else 0"
      e @?= Raw.progMap Map.! "if True then 1 else 0"
  , testCase "2 * 3 == 6" $ do
      e <- parseProg "2 * 3 == 6"
      e @?= Raw.progMap Map.! "2 * 3 == 6"
  ]

scopeTests :: TestTree
scopeTests = testGroup "Scope tests"
  [ testCase "\\x -> x * x" $ do
      e1 <- parseProg "\\x -> x * x"
      e2 <- r2lProg e1
      case L.stripAnn e2 of
        L.ELam x0 (L.EApp (L.EApp _ (L.EVar x1)) (L.EVar x2)) -> do x0 @?= x1; x1 @?= x2
        e                                                     -> assertFailure $ "unexpected: " ++ show e
  ]

stepTests :: TestTree
stepTests = testGroup "Step tests"
  [ testCaseSteps "42" $ \step -> do
      step "Parser"
      e1 <- parseProg "42"
      e1 @?= Raw.progMap Map.! "42"
      step "RawLam"
      e2 <- r2lProg e1
      e2 @?= Lambda.progMap Map.! "42"
      step "Lambda.Tc"
      Lambda.tcProg e2
      step " LamAnf"
      let e3 = l2aProg e2
      e3 @?= Anf.progMap Map.! "42"
      step "Anf.Tc"
      Anf.tcProg e3
      step "AnfClos"
      e4 <- a2cProg e3
      e4 @?= Closure.progMap Map.! "42"
      step "Closure.Tc"
      Closure.tcProg e4
      step "Done"
  , testCaseSteps "\\x -> x" $ \step -> do
      e1 <- parseProg "\\x -> x"
      step "Lambda.Tc"
      e2 <- r2lProg e1
      Lambda.tcProg e2
      step "Anf.Tc"
      let e3 = l2aProg e2
      Anf.tcProg e3
      step "Closure.Tc"
      e4 <- a2cProg e3
      Closure.tcProg e4
      step "Done"
  , testCaseSteps "let x = 42 in x" $ \step -> do
      e1 <- parseProg "let x = 42 in x"
      step "Lambda.Tc"
      e2 <- r2lProg e1
      Lambda.tcProg e2
      step "Anf.Tc"
      let e3 = l2aProg e2
      Anf.tcProg e3
      step "Closure.Tc"
      e4 <- a2cProg e3
      Closure.tcProg e4
      step "Done"
  , testCaseSteps "(\\x -> x) 5" $ \step -> do
      e1 <- parseProg "(\\x -> x) 5"
      step "Lambda.Tc"
      e2 <- r2lProg e1
      Lambda.tcProg e2
      step "Anf.Tc"
      let e3 = l2aProg e2
      Anf.tcProg e3
      step "Closure.Tc"
      e4 <- a2cProg e3
      Closure.tcProg e4
      step "Done"
  , testCaseSteps "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1" $ \step -> do
      e1 <- parseProg "let double = \\f -> \\x -> f (f x) in let add5 = \\x -> x + 5 in double add5 1"
      step "Lambda.Tc"
      e2 <- r2lProg e1
      Lambda.tcProg e2
      step "Anf.Tc"
      let e3 = l2aProg e2
      Anf.tcProg e3
      step "Closure.Tc"
      e4 <- a2cProg e3
      Closure.tcProg e4
      step "Done"
  ]

outputClosString :: Text -> IO BL.ByteString
outputClosString  inp = do
  raw_prog <- parseProg inp
  lam_prog <- r2lProg raw_prog
  let anf_prog = l2aProg lam_prog
  clos_prog <- a2cProg anf_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pretty clos_prog
  return $ BL.fromStrict $ encodeUtf8 out

outputUntyString :: Text -> IO BL.ByteString
outputUntyString  inp = do
  raw_prog <- parseProg inp
  lam_prog <- r2lProg raw_prog
  let anf_prog = l2aProg lam_prog
  clos_prog <- a2cProg anf_prog
  let unty_prog = c2uProg clos_prog
  let layoutOptions = defaultLayoutOptions{layoutPageWidth = AvailablePerLine 80 1.0}
  let out = renderStrict $ layoutPretty layoutOptions $ pretty unty_prog
  return $ BL.fromStrict $ encodeUtf8 out

goldenTests :: TestTree
goldenTests = testGroup "Golden tests"
  [ goldenVsString "\\x -> x" ".golden/clos_\\x -> x.txt" $ outputClosString "\\x -> x"
  , goldenVsString "(\\x -> x) 5" ".golden/clos_(\\x -> x) 5.txt" $ outputClosString "(\\x -> x) 5"
  , goldenVsString "\\x -> (\\y -> x + y)" ".golden/clos_\\x -> (\\y -> x + y).txt" $ outputClosString "\\x -> (\\y -> x + y)"
  , goldenVsString "\\x -> x" ".golden/unty_\\x -> x.txt" $ outputUntyString "\\x -> x"
  , goldenVsString "(\\x -> x) 5" ".golden/unty_(\\x -> x) 5.txt" $ outputUntyString "(\\x -> x) 5"
  , goldenVsString "\\x -> (\\y -> x + y)" ".golden/unty_\\x -> (\\y -> x + y).txt" $ outputUntyString "\\x -> (\\y -> x + y)"]
