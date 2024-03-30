module PisigmaTal.Idx (
    Idx(..),
    pattern Idx2
) where

import Prettyprinter.Prec

data Idx
    = Idx1
    | IdxS Idx
    deriving (Eq, Ord)

pattern Idx2 :: Idx
pattern Idx2 = IdxS Idx1

instance Show Idx where
    show idx = "#" ++ show (fromEnum idx)

instance PrettyPrec Idx where
    pretty idx = pretty (fromEnum idx)

instance Num Idx where
    Idx1 + idx2      = idx2
    IdxS idx1 + idx2 = idx1 + IdxS idx2
    Idx1 * idx2      = idx2
    IdxS idx1 * idx2 = idx1 * idx2 + idx2
    fromInteger n | n < 1 = error "non-positive index"
    fromInteger 1 = Idx1
    fromInteger n = IdxS (fromInteger (n - 1))
    abs = id
    signum Idx1 = 1
    signum _    = 1
    negate _ = error "cannot negate"

instance Enum Idx where
    toEnum n | n < 1 = error "non-positive index"
    toEnum 1 = Idx1
    toEnum n = IdxS (toEnum (n - 1))
    fromEnum Idx1       = 1
    fromEnum (IdxS idx) = 1 + fromEnum idx
