module PisigmaTal.Primitive (PrimOp) where

import Prettyprinter.Prec

data PrimOp = Add | Sub | Mul | Div
    deriving (Eq, Show)

instance PrettyPrec PrimOp where
    prettyPrec _ Add = "+"
    prettyPrec _ Sub = "-"
    prettyPrec _ Mul = "*"
    prettyPrec _ Div = "/"
