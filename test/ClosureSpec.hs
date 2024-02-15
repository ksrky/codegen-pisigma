module ClosureSpec (spec) where

import AnfToClos
import Closure
import Data.Text
import LamToAnf
import Parser
import RawToLam
import Test.Hspec
import Prettyprinter.Prec

textToClosure :: Text -> IO Prog
textToClosure inp = do
    raw_prog <- parseProg inp
    lam_prog <- r2lProg raw_prog
    let clos_prog = l2aProg lam_prog
    stripAnn <$> a2cProg clos_prog

spec :: Spec
spec = do
  describe "textToClosure" $ do
    it "42" $
      textToClosure "42" `shouldReturn` ([], ERet (VValTy (VLit (LInt 42)) TInt))
    it "\\x -> x" $ do
      res <- textToClosure "\\x -> x"
      print $ pretty res
      res `shouldBe` ([], ERet (VValTy (VLit (LInt 42)) TInt))
        {-([Def {
            name = (x_code,TFun [TRec tv_cl (TRow (TFun [TVar tv_cl,TInt] TInt :> REmpty)),TInt] TInt),
            args = [(x_cl,TRec tv_cl (TRow (TFun [TVar tv_cl,TInt] TInt :> REmpty))),(x,TInt)],
            body = ERet (VVar (x,TInt))}]
        , ERet (
            VPack (TRow REmpty)
              (VRoll (VTuple [VGlb ("x_code",TFun [TRec "tv_cl" (TRow (TFun [TVar "tv_cl",TInt] TInt :> REmpty)),TInt] TInt)]) (TRec tv_cl (TRow (TFun [TVar tv_cl,TInt] TInt :> REmpty)))) (TRec tv_cl (TRow (TFun [TVar tv_cl,TInt] TInt :> REmpty)))))
-}
