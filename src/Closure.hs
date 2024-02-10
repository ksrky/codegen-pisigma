{-# LANGUAGE TemplateHaskell #-}

module Closure (
    Lit(..),
    Ty(..),
    Var,
    Val(..),
    Dec(..),
    Exp(..),
    Def(..),
    Prog,
    Typeable(..)) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack
import Id
import Prettyprinter            hiding (Pretty (..))
import Prettyprinter.Prec

newtype Lit = LInt Int
    deriving (Eq, Show)

type TyVar = Id

data Ty
    = TInt
    | TVar TyVar
    | TFun [Ty] Ty
    | TTuple [Ty]
    | TEx TyVar Ty
    deriving (Eq, Show)

type Var = (Id, Ty)

data Val
    = VLit Lit
    | VVar Var
    | VGlb Var
    | VTuple [Val]
    | VPack Ty Val Ty
    | VValTy Val Ty
    deriving (Eq, Show)

data Dec
    = DVal Var Val
    | DCall Var Val [Val]
    | DProj Var Val Int
    | DUnpack TyVar Var Val
    deriving (Eq, Show)

data Exp
    = ELet Dec Exp
    | ERet Val
    | EExpTy Exp Ty
    deriving (Eq, Show)

data Def = Def {name :: Var, args :: [Var], body :: Exp}
    deriving (Eq, Show)

type Prog = ([Def], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

class Typeable a where
    typeof :: HasCallStack => a -> Ty

instance Typeable Lit where
    typeof _ = TInt

instance Typeable Ty where
    typeof = id

instance Typeable Var where
    typeof = snd

instance Typeable Val where
    typeof = cata $ \case
        VLitF l -> typeof l
        VVarF x -> typeof x
        VGlbF f -> typeof f
        VTupleF ts -> TTuple ts
        VPackF _ _ t -> t
        VValTyF _ t -> t

instance Typeable Dec where
    typeof (DVal _ v) = typeof v
    typeof (DCall _ e1 _) = case typeof e1 of
        TFun _ t12 -> t12
        _          -> error "impossible"
    typeof (DProj _ v i) = case typeof v of
        TTuple ts -> ts !! i
        _         -> error "impossible"
    typeof (DUnpack _ _ v) = case typeof v of
        TEx _ t -> t
        _       -> error "impossible"

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ t -> t
        ERetF v -> typeof v
        EExpTyF _ t -> t

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Ty where
    prettyPrec _ TInt = "Int"
    prettyPrec _ (TVar tv) = pretty tv
    prettyPrec p (TFun ts t) = parPrec p 2 $
        parens (hsep $ punctuate "," $ map (prettyPrec 1) ts) <+> "->" <+> prettyPrec 2 t
    prettyPrec _ (TTuple ts) = angles $ hsep $ punctuate "," $ map pretty ts
    prettyPrec p (TEx tv t) = parPrec p 0 $ "∃" <+> pretty tv <> "." <+> pretty t

instance PrettyPrec Var where
    pretty (x, t) = pretty x <+> ":" <+> pretty t

instance PrettyPrec Val where
    prettyPrec _ (VLit l) = pretty l
    prettyPrec _ (VVar (x, _)) = pretty x
    prettyPrec _ (VGlb (f, _)) = pretty f
    prettyPrec _ (VTuple vs) = angles $ hsep $ punctuate "," $ map pretty vs
    prettyPrec p (VPack t1 v t2) =
        parPrec p 0 $ hsep ["pack", brackets (pretty t1 <> "," <+> pretty v), "as", pretty t2]
    prettyPrec _ (VValTy v t) = parens $ hsep [pretty v, ":", pretty t]

instance PrettyPrec Dec where
    pretty (DVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (DCall x v1 vs2) = pretty x <+> "=" <> softline <> pretty v1
        <+> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (DProj x v i) = pretty x <+> "=" <> softline <> pretty v <> "." <> pretty i
    pretty (DUnpack tv x v) = brackets (pretty tv <> "," <+> pretty x)
        <+> "=" <> softline <> "unpack" <+> prettyMax v

instance PrettyPrec Exp where
    pretty (ELet d e)   = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ERet v)     = "ret" <+> prettyMax v
    pretty (EExpTy e t) = parens $ pretty e <+> ":" <+> pretty t

instance PrettyPrec Def where
    pretty (Def (f, _) xs e) = pretty f <+> hsep (map (parens . pretty) xs) <+> "=" <+> align (pretty e)

instance PrettyPrec Prog where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e
