module Untyped (
    Lit(..),
    Val(..),
    Bind(..),
    Exp(..),
    Defn(..),
    Program,) where

import Id
import Prettyprinter      hiding (pretty)
import Prettyprinter.Prec

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Val
    = VLit Lit
    | VVar Id
    | VFunc Id
    | VLabel Label
    | VTuple [Val]
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [(Label, Exp)]
    | EReturn Val
    deriving (Eq, Show)

data Bind
    = BVal Id Val
    | BCall Id Val [Val]
    | BProj Id Val Int
    deriving (Eq, Show)

data Defn = Defn {name :: Id, args :: [Id], body :: Exp}

type Program = ([Defn], Exp)

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Val where
    prettyPrec _ (VLit l)    = pretty l
    prettyPrec _ (VVar x)    = pretty x
    prettyPrec _ (VFunc f)   = pretty f
    prettyPrec _ (VLabel l)  = pretty l
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
    pretty (EReturn v)   = "ret" <+> prettyMax v

instance PrettyPrec Defn where
    pretty (Defn f xs e) = pretty f <+> hsep (map pretty xs) <+> "=" <+> align (pretty e)

instance PrettyPrec Program where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e
