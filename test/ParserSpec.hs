module ParserSpec (spec) where

import Parser
import Raw
import Test.Hspec

spec :: Spec
spec = do
  describe "parseProg" $ do
    it "42" $
      parseProg "42" `shouldReturn` ELit (LInt 42)
    it "x" $
      parseProg "x" `shouldReturn` EVar "x"
    it "f x" $
      parseProg "f x" `shouldReturn` EApp (EVar "f") (EVar "x")
    it "\\x:Int -> x" $
      parseProg "\\x -> x" `shouldReturn` ELam "x" (EVar "x")
    it "let x = 42 in x" $
      parseProg "let x = 42 in x" `shouldReturn` ELet [("x", ELit (LInt 42))] (EVar "x")
    it "let rec f = \\x -> g x and g = \\x -> f x in f 0" $
      parseProg "let rec f = \\x -> g x and g = \\x -> f x in f 0" `shouldReturn`
        ELetrec [
          ("f", ELam "x" (EApp (EVar "g") (EVar "x"))),
          ("g", ELam "x" (EApp (EVar "f") (EVar "x")))]
          (EApp (EVar "f") (ELit (LInt 0)))
