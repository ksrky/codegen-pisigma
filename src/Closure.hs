{-# LANGUAGE TemplateHaskell #-}

module Closure (
    Lit(..),
    TyVar,
    Ty(..),
    TyF(..),
    Row(..),
    Var,
    Val(..),
    ValF(..),
    Dec(..),
    Exp(..),
    ExpF(..),
    Def(..),
    Prog,
    Typeable(..),
    StripAnn(..),
    mkTTuple) where

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
    | TEx TyVar Ty
    | TRec TyVar Ty
    | TRow Row
    deriving (Eq, Show)

data Row = REmpty | RVar TyVar | Ty :> Row
    deriving (Eq, Show)

type Var = (Id, Ty)

data Val
    = VLit Lit
    | VVar Var
    | VGlb Var
    | VTuple [Val]
    | VPack Ty Val Ty
    | VRoll Val Ty
    | VUnroll Val
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
        VTupleF vs -> TRow $ foldr ((:>) . typeof) REmpty vs
        VPackF _ _ t -> t
        VRollF _ t -> t
        VUnrollF v -> case v of
            TRec tv t -> (cata $ \case
                TVarF tv' | tv == tv' -> v
                t' -> embed t') t
            _ -> error "impossible"
        VValTyF _ t -> t

instance Typeable Dec where
    typeof (DVal _ v) = typeof v
    typeof (DCall _ e1 _) = case typeof e1 of
        TFun _ t12 -> t12
        _          -> error "impossible"
    typeof (DProj _ v i) = case typeof v of
        TRow r -> go i r
        _      -> error "impossible"
      where
        -- Make sure not to generate empty environements.
        go 1 (t :> _) = t
        go n (_ :> r) = go (n - 1) r
        go _ _        = error "impossible"
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
    prettyPrec p (TEx tv t) = parPrec p 0 $ "∃" <+> pretty tv <> "." <+> pretty t
    prettyPrec p (TRec tv t) = parPrec p 0 $ "μ" <+> pretty tv <> "." <+> pretty t
    prettyPrec _ (TRow r) = angles $ pretty r

instance PrettyPrec Row where
    pretty REmpty    = "ε"
    pretty (RVar tv) = pretty tv
    pretty (t :> r)  = pretty t <> ";" <+> pretty r

instance PrettyPrec Var where
    pretty (x, t) = pretty x <+> ":" <+> pretty t

instance PrettyPrec Val where
    prettyPrec _ (VLit l) = pretty l
    prettyPrec _ (VVar (x, _)) = pretty x
    prettyPrec _ (VGlb (f, _)) = pretty f
    prettyPrec _ (VTuple vs) = angles $ hsep $ punctuate "," $ map pretty vs
    prettyPrec p (VPack t1 v t2) =
        parPrec p 0 $ hsep ["pack", brackets (pretty t1 <> "," <+> pretty v), "as", prettyPrec 2 t2]
    prettyPrec p (VRoll v t) = parPrec p 0 $ "roll" <+> prettyMax v <+> "as" <+> prettyPrec 2 t
    prettyPrec p (VUnroll v) = parPrec p 0 $ "unroll" <+> prettyPrec 1 v
    prettyPrec _ (VValTy v t) = parens $ hsep [pretty v, ":", pretty t]

instance PrettyPrec Dec where
    pretty (DVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (DCall x v1 vs2) = pretty x <+> "=" <> softline <> prettyMax v1
        <+> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (DProj x v i) = pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty i
    pretty (DUnpack tv x v) = brackets (pretty tv <> "," <+> pretty x)
        <+> "=" <> softline <> "unpack" <+> prettyMax v

instance PrettyPrec Exp where
    pretty (ELet d e)   = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ERet v)     = "ret" <+> prettyMax v
    pretty (EExpTy e t) = parens $ pretty e <+> ":" <+> pretty t

instance PrettyPrec Def where
    pretty (Def (f, _) xs e) = pretty f <+> hsep (map (parens . pretty) xs) <+> "=" <> line <> indent 2 (pretty e)

instance PrettyPrec Prog where
    pretty (ds, e) = vsep (map pretty ds) <> line <> pretty e

class StripAnn a where
    stripAnn :: a -> a

instance StripAnn Val where
    stripAnn = cata $ \case
        VValTyF v _ -> v
        v           -> embed v

instance StripAnn Dec where
    stripAnn (DVal x v)       = DVal x (stripAnn v)
    stripAnn (DCall x v vs)   = DCall x (stripAnn v) (map stripAnn vs)
    stripAnn (DProj x v i)    = DProj x (stripAnn v) i
    stripAnn (DUnpack tv x v) = DUnpack tv x (stripAnn v)

instance StripAnn Exp where
    stripAnn = cata $ \case
        ELetF d e -> ELet (stripAnn d) (stripAnn e)
        ERetF v   -> ERet (stripAnn v)
        EExpTyF e _ -> e

instance StripAnn Def where
    stripAnn (Def f xs e) = Def f xs (stripAnn e)

instance StripAnn Prog where
    stripAnn (ds, e) = (map stripAnn ds, stripAnn e)

mkTTuple :: [Ty] -> Ty
mkTTuple ts = TRow $ foldr (:>) REmpty ts
