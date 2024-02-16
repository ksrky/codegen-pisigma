module Tests.Lambda (progMap) where

import Data.Map.Strict qualified as Map
import Lambda

progMap :: Map.Map String Prog
progMap = Map.fromList
  [ ("42", EExpTy (ELit (LInt 42)) TInt)
  ]
