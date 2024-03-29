module PisigmaTal.Primitive (PrimOp(..)) where

import Prettyprinter.Prec

data PrimOp = Add | Sub | Mul | Div
    deriving (Eq, Show)

instance PrettyPrec PrimOp where
    prettyPrec _ Add = "#add"
    prettyPrec _ Sub = "#sub"
    prettyPrec _ Mul = "#mul"
    prettyPrec _ Div = "#div"
