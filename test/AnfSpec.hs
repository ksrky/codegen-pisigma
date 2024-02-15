module AnfSpec (spec) where

import Anf
import Anf.Tc
import Data.Text
import LamToAnf
import Parser
import RawToLam
import Test.Hspec

textToAnf :: Text -> IO Exp
textToAnf inp = do
  raw_prog <- parseProg inp
  lam_prog <- r2lProg raw_prog
  return $ l2aProg lam_prog

tcAnf :: Text -> IO ()
tcAnf inp = tcProg =<< textToAnf inp

spec :: Spec
spec = do
  describe "textToLambda" $ do
    it "42" $
      textToAnf "42" `shouldReturn` ERet (VValTy (VLit (LInt 42)) TInt)
  describe "tcLambda" $ do
    it "\\x -> x" $
      tcAnf "\\x -> x" `shouldReturn` ()
    it "let x = 42 in x" $
      tcAnf "let x = 42 in x" `shouldReturn` ()
