module Untyped (
    Lit(..),
    Val(..),
    Bind(..),
    Exp(..),
    Def(..),
    Prog,) where

import Id
import Prettyprinter      hiding (pretty)
import Prettyprinter.Prec

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Val
    = VLit Lit
    | VVar Id
    | VGlb Id
    | VLab Label
    | VTuple [Val]
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [(Label, Exp)]
    | ERet Val
    deriving (Eq, Show)

data Bind
    = BVal Id Val
    | BCall Id Val [Val]
    | BProj Id Val Int
    deriving (Eq, Show)

data Def = Def {name :: Id, args :: [Id], body :: Exp}

type Prog = ([Def], Exp)

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Val where
    prettyPrec _ (VLit l)    = pretty l
    prettyPrec _ (VVar x)    = pretty x
    prettyPrec _ (VGlb f)    = pretty f
    prettyPrec _ (VLab l)    = pretty l
    prettyPrec _ (VTuple vs) = brackets $ hsep $ punctuate ";" $ map pretty vs

instance PrettyPrec Bind where
    pretty (BVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (BCall x v1 vs2) = pretty x <+> "=" <> softline <> prettyMax v1
        <> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (BProj x v i) = pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty i

instance PrettyPrec Exp where
    pretty (ELet d e) = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ECase v les) = vsep [ "case" <+> pretty v <+> "of"
                                , "  " <> align (vsep (map (\(li, ei) -> hang 2 $ sep [pretty li <+> "->", pretty ei]) les))]
    pretty (ERet v)   = "ret" <+> prettyMax v

instance PrettyPrec Def where
    pretty (Def f xs e) = pretty f <+> hsep (map pretty xs) <+> "=" <+> align (pretty e)

instance PrettyPrec Prog where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e
