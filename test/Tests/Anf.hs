module Tests.Anf (expMap) where

import Anf

import Data.Map.Strict qualified as Map

expMap :: Map.Map String Exp
expMap =  Map.fromList
  [ ("42", EReturn (VAnnot (VLit (LInt 42)) TInt))
  ]
