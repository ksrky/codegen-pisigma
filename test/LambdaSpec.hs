module LambdaSpec (spec) where

import Data.Text
import Lambda
import Lambda.Tc
import Parser
import RawToLam
import Test.Hspec

textToLambda :: Text -> IO Exp
textToLambda inp = do
  raw_prog <- parseProg inp
  r2lProg raw_prog

tcLambda :: Text -> IO ()
tcLambda inp = tcProg =<< textToLambda inp

spec :: Spec
spec = do
  describe "textToLambda" $ do
    it "42" $
      textToLambda "42" `shouldReturn` EExpTy (ELit (LInt 42)) TInt
  describe "tcLambda" $ do
    it "\\x -> x" $
      tcLambda "\\x -> x" `shouldReturn` ()
    it "let x = 42 in x" $
      tcLambda "let x = 42 in x" `shouldReturn` ()
