{-# LANGUAGE PatternSynonyms #-}

module Tests.Lambda (expMap, pattern ScopeTest1) where

import Lambda

import Data.Map.Strict qualified as Map


expMap :: Map.Map String Exp
expMap = Map.fromList
  [ ("42", EAnnot (ELit (LInt 42)) TInt)
  ]


pattern ScopeTest1 :: Var -> Var -> Var -> Exp
pattern ScopeTest1 x0 x1 x2 <- ELam x0 (EApp _ (ETuple [EVar x1, EVar x2]))
