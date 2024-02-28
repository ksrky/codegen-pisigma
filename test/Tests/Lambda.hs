module Tests.Lambda (expMap) where

import Data.Map.Strict qualified as Map
import Lambda

expMap :: Map.Map String Exp
expMap = Map.fromList
  [ ("42", EAnnot (ELit (LInt 42)) TInt)
  ]
