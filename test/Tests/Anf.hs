module Tests.Anf (progMap) where

import Anf
import Data.Map.Strict qualified as Map

progMap :: Map.Map String Prog
progMap =  Map.fromList
  [ ("42", ERet (VValTy (VLit (LInt 42)) TInt))
  ]
