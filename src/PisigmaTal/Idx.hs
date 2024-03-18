module PisigmaTal.Idx (
    Idx(..),
    idxToInt,
    intToIdx,
    idxPred
) where

import Prettyprinter.Prec

data Idx
    = Idx1
    | IdxS Idx
    deriving (Eq, Ord)

instance Show Idx where
    show idx = "#" ++ show (idxToInt idx)

instance PrettyPrec Idx where
    pretty idx = pretty (idxToInt idx)

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
    negate Idx1       = Idx1
    negate (IdxS idx) = IdxS (negate idx)

idxToInt :: Idx -> Int
idxToInt Idx1       = 1
idxToInt (IdxS idx) = 1 + idxToInt idx

intToIdx :: Int -> Idx
intToIdx n | n < 1 = error "non-positive index"
intToIdx 1 = Idx1
intToIdx n = IdxS (intToIdx (n - 1))



idxPred :: Idx -> Idx
idxPred Idx1       = Idx1
idxPred (IdxS idx) = idx
