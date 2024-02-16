{-# LANGUAGE TemplateHaskell #-}

module Closure (
    Lit(..),
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
    substTop,
    getDecVar,
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

data Ty
    = TInt
    | TVar Int
    | TFun [Ty] Ty
    | TEx Ty
    | TRec Ty
    | TRow Row
    deriving (Eq, Show)

data Row = REmpty | RVar Int | Ty :> Row
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
    | DUnpack Var Val -- ignoring the type variable
    deriving (Eq, Show)

data Exp
    = ELet Dec Exp
    | ERet Val
    | EExpTy Exp Ty
    deriving (Eq, Show)

data Def = Def {code :: Var, args :: [Var], body :: Exp}
    deriving (Eq, Show)

type Prog = ([Def], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

substTop :: Ty -> Ty -> Ty
substTop s = cata $ \case
    TVarF 0 -> s
    TVarF i -> TVar (i - 1)
    t -> embed t

getDecVar :: Dec -> Var
getDecVar (DVal x _)    = x
getDecVar (DCall x _ _) = x
getDecVar (DProj x _ _) = x
getDecVar (DUnpack x _) = x

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
        VUnrollF v ->
            case typeof v of
                TRec t -> substTop (TRec t) t
                _      -> error "required recursive type"
        VValTyF _ t -> t

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
    prettyPrec p (TEx t) = parPrec p 0 $ "∃." <+> pretty t
    prettyPrec p (TRec t) = parPrec p 0 $ "μ." <+> pretty t
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
    pretty (DUnpack x v) = brackets ("_," <+> pretty x) <+> "=" <> softline <> "unpack" <+> prettyMax v

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
    stripAnn (DVal x v)     = DVal x (stripAnn v)
    stripAnn (DCall x v vs) = DCall x (stripAnn v) (map stripAnn vs)
    stripAnn (DProj x v i)  = DProj x (stripAnn v) i
    stripAnn (DUnpack x v)  = DUnpack x (stripAnn v)

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
