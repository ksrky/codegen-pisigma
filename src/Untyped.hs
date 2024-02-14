module Untyped (
    Lit(..),
    Val(..),
    Dec(..),
    Exp(..),
    Def(..),
    Prog,) where

import Id
import Prettyprinter      hiding (pretty)
import Prettyprinter.Prec

newtype Lit = LInt Int
    deriving (Eq, Show)

data Val
    = VLit Lit
    | VVar Id
    | VGlb Id
    | VTuple [Val]
    deriving (Eq, Show)

data Exp
    = ELet Dec Exp
    | ERet Val
    deriving (Eq, Show)

data Dec
    = DVal Id Val
    | DCall Id Val [Val]
    | DProj Id Val Int
    deriving (Eq, Show)

data Def = Def {name :: Id, args :: [Id], body :: Exp}

type Prog = ([Def], Exp)

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Val where
    prettyPrec _ (VLit l)    = pretty l
    prettyPrec _ (VVar x)    = pretty x
    prettyPrec _ (VGlb f)    = pretty f
    prettyPrec _ (VTuple vs) = angles $ hsep $ punctuate "," $ map pretty vs

instance PrettyPrec Dec where
    pretty (DVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (DCall x v1 vs2) = pretty x <+> "=" <> softline <> prettyMax v1
        <+> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (DProj x v i) = pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty i

instance PrettyPrec Exp where
    pretty (ELet d e) = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ERet v)   = "ret" <+> prettyMax v

instance PrettyPrec Def where
    pretty (Def f xs e) = pretty f <+> hsep (map pretty xs) <+> "=" <+> align (pretty e)

instance PrettyPrec Prog where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e
