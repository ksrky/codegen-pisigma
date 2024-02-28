module Tests.Raw (expMap) where

import Raw

import Data.Map.Strict qualified as Map

expMap :: Map.Map String Exp
expMap = Map.fromList
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
  , ("True", ELabel "True")
  , ("if True then 1 else 0", EIf (ELabel "True") (ELit (LInt 1)) (ELit (LInt 0)))
  , ("2 * 3 == 6", EBinOp "==" (EBinOp "*" (ELit (LInt 2)) (ELit (LInt 3))) (ELit (LInt 6)))
  ]
