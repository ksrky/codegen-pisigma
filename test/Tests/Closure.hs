module Tests.Closure (progMap) where

import Closure
import Data.Map.Strict qualified as Map

progMap :: Map.Map String Prog
progMap = Map.fromList
  [ ("42", ([], ERet (VValTy (VLit (LInt 42)) TInt)))
  ]
