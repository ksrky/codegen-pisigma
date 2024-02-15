{-# LANGUAGE TemplateHaskell #-}

module Raw (
    Lit(..),
    Exp(..),
    Prog) where

import Data.Functor.Foldable.TH

newtype Lit = LInt Int
    deriving (Eq, Show)

data Exp
    = ELit Lit
    | EVar String
    | EApp Exp Exp
    | ELam String Exp
    | EBinOp String Exp Exp
    | ELet [(String, Exp)] Exp
    | ELetrec [(String, Exp)] Exp
    deriving (Eq, Show)

type Prog = Exp

makeBaseFunctor ''Exp
