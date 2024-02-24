module Tests.Raw (progMap) where

import Data.Map.Strict qualified as Map
import Raw

progMap :: Map.Map String Prog
progMap = Map.fromList
  [ ("42", ELit (LInt 42))
  , ("x", EVar "x")
  , ("f x", EApp (EVar "f") (EVar "x"))
  , ("\\x -> x", ELam "x" (EVar "x"))
  , ("let x = 42 in x", ELet [("x", ELit (LInt 42))] (EVar "x"))
  , ("let rec f = \\x -> g x and g = \\x -> f x in f 0",
     ELetrec [
       ("f", ELam "x" (EApp (EVar "g") (EVar "x"))),
       ("g", ELam "x" (EApp (EVar "f") (EVar "x")))]
       (EApp (EVar "f") (ELit (LInt 0))) )
  , ("True", ELab "True")
  , ("if True then 1 else 0", EIf (ELab "True") (ELit (LInt 1)) (ELit (LInt 0)))
  , ("2 * 3 == 6", EBinOp "==" (EBinOp "*" (ELit (LInt 2)) (ELit (LInt 3))) (ELit (LInt 6)))
  ]
