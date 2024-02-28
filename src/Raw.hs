{-# LANGUAGE TemplateHaskell #-}

module Raw (
    Lit(..),
    Exp(..),
    Program
) where

import Data.Functor.Foldable.TH

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Exp
    = ELit Lit
    | EVar String
    | ELabel Label
    | EApp Exp Exp
    | ELam String Exp
    | EBinOp String Exp Exp
    | ELet [(String, Exp)] Exp
    | ELetrec [(String, Exp)] Exp
    | EIf Exp Exp Exp
    deriving (Eq, Show)

type Program = Exp

makeBaseFunctor ''Exp
