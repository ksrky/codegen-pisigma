module Tests.PisigmaTal.Lambda (expMap, pattern ScopeTest1) where

import Data.Map.Strict   qualified as Map
import PisigmaTal.Lambda


expMap :: Map.Map String Exp
expMap = Map.fromList
  [ ("42", EAnnot (ELit (LInt 42)) TInt)
  ]


pattern ScopeTest1 :: Var -> Var -> Var -> Exp
pattern ScopeTest1 x0 x1 x2 <- ELam x0 (EExternApp _ [EVar x1, EVar x2])
