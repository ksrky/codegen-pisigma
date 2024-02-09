module Prettyprinter.Prec (
    PrettyPrec(..),
    prettyMax,
    parPrec) where

import Prettyprinter hiding (pretty)
import Prettyprinter qualified

class PrettyPrec a where
    pretty :: a -> Doc ann
    pretty = prettyPrec 0
    prettyPrec :: Int -> a -> Doc ann
    prettyPrec _ = pretty

instance PrettyPrec String where
    pretty = Prettyprinter.pretty

instance PrettyPrec Int where
    pretty = Prettyprinter.pretty

maxPrec :: Int
maxPrec = 9

prettyMax :: PrettyPrec a => a -> Doc ann
prettyMax = prettyPrec maxPrec

parPrec :: Int -> Int -> Doc ann -> Doc ann
parPrec p n = if p > n then parens else id
