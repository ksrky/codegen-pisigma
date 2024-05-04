module Tests.PisigmaTal.Lambda
  ( expMap
  , pattern ScopeTest1
  , pattern ScopeTest2
  , pattern ScopeTest3
  ) where

import Data.Map.Strict   qualified as Map
import PisigmaTal.Lambda

expMap :: Map.Map String Exp
expMap = Map.fromList
  [ ("42", EAnnot (ELit (LInt 42)) TInt)
  ]

pattern ScopeTest1 :: Var -> Var -> Var -> Exp
pattern ScopeTest1 x0 x1 x2 <- ELam x0 (EFullApp _ [EVar x1, EVar x2])

pattern ScopeTest2 :: Var -> Var -> Var -> Exp
pattern ScopeTest2 x0 x1 x2 <- ELam x0 (ELam x1 (EVar x2))

pattern ScopeTest3 :: Var -> Var -> Var -> Exp
pattern ScopeTest3 x y0 y1 <- ELam x (ELam y0 (EVar y1))
