{-# LANGUAGE TemplateHaskell #-}

module Closure (
    Lit(..),
    TyVar,
    Ty(..),
    TyF(..),
    RowTy(..),
    Var,
    Val(..),
    ValF(..),
    Bind(..),
    Exp(..),
    ExpF(..),
    Dec(..),
    Defn(..),
    Program,
    Env,
    lookupEnumEnv,
    lookupBindEnv,
    extendBindEnv,
    unrollUClos,
    unpackClos,
    bindVar,
    Typeable(..),
    StripAnn(..),
    mkClos,
    mkUClos,
    mkTTuple
) where

import Id

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack

import Prettyprinter            hiding (Pretty (..))
import Prettyprinter.Prec

type TyVar = Id

data Ty
    = TInt
    | TVar TyVar
    | TName Id
    | TFun [Ty] Ty
    | TExists TyVar Ty
    | TRecurs TyVar Ty
    | TRow RowTy
    deriving (Eq, Show)

data RowTy = REmpty | RVar TyVar | Ty :> RowTy
    deriving (Eq, Show)

newtype Lit = LInt Int
    deriving (Eq, Show)

type Var = (Id, Ty)

type Label = String

data Val
    = VLit Lit
    | VVar Var
    | VFun Var
    | VLabel Label Ty
    | VTuple [Val]
    | VPack Ty Val Ty
    | VRoll Val Ty
    | VUnroll Val
    | VAnnot Val Ty
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BCall Var Val [Val]
    | BProj Var Val Int
    | BUnpack TyVar Var Val
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [(Label, Exp)]
    | EReturn Val
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Dec
    = DEnum Id [Label]
    | DBind Id Ty
    deriving (Eq, Show)

data Defn = Defn {code :: Var, args :: [Var], body :: Exp}
    deriving (Eq, Show)

type Program = ([Dec], [Defn], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''RowTy
makeBaseFunctor ''Val
makeBaseFunctor ''Exp

type Env = [Dec]

lookupEnumEnv :: MonadFail m => Id -> Env -> m [Label]
lookupEnumEnv x = \case
    DEnum y ls : _ | x == y -> return ls
    _ : env -> lookupEnumEnv x env
    [] -> fail "Enum not found"

lookupBindEnv :: MonadFail m => Id -> Env -> m Ty
lookupBindEnv x = \case
    DBind y t : _ | x == y -> return t
    _ : env -> lookupBindEnv x env
    [] -> fail "Func not found"

extendBindEnv :: Var -> Env -> Env
extendBindEnv (x, t) = (DBind x t:)

-- | Make a packed closure
mkClos :: [Ty] -> Ty -> Ty
mkClos ts1 t2 =
    let t_env = newIdUnsafe "t_env"
        t_cl = newIdUnsafe "t_cl" in
    TExists t_env $ TRecurs t_cl $ TRow $ TFun (TVar t_cl : ts1) t2 :> RVar t_env

-- | Make an unpacked closure
mkUClos :: [Ty] -> Ty -> RowTy -> Ty
mkUClos ts1 t2 r =
    let t_cl = newIdUnsafe "t_cl" in
    TRecurs t_cl $ TRow (TFun (TVar t_cl : ts1) t2 :> r)

mkTTuple :: [Ty] -> Ty
mkTTuple ts = TRow $ foldr (:>) REmpty ts

unrollUClos :: Ty -> Ty -> Ty
unrollUClos s (TRow (TFun (TVar _: ts1) t2 :> r)) = TRow $ TFun (s : ts1) t2 :> r
unrollUClos _ _                                   = error "impossible"

unpackClos :: Ty -> Ty -> Ty
unpackClos (TRow r) (TRecurs tv (TRow (TFun ts1 t2 :> RVar _))) = TRecurs tv $ TRow $ TFun ts1 t2 :> r
unpackClos  _ _                                           = error "impossible"

bindVar :: Bind -> Var
bindVar (BVal x _)      = x
bindVar (BCall x _ _)   = x
bindVar (BProj x _ _)   = x
bindVar (BUnpack _ x _) = x

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
        VFunF f -> typeof f
        VLabelF _ t -> t
        VTupleF vs -> TRow $ foldr ((:>) . typeof) REmpty vs
        VPackF _ _ t -> t
        VRollF _ t -> t
        VUnrollF v ->
            case typeof v of
                TRecurs tv t -> unrollUClos (TRecurs tv t) t
                _            -> error "required recursive type"
        VAnnotF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ t -> t
        ECaseF _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EReturnF v -> typeof v
        EAnnotF _ t -> t

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Ty where
    prettyPrec _ TInt = "Int"
    prettyPrec _ (TVar tv) = pretty tv
    prettyPrec _ (TName l) = pretty l
    prettyPrec p (TFun ts t) = parPrec p 2 $
        parens (hsep $ punctuate "," $ map (prettyPrec 1) ts) <+> "->" <+> prettyPrec 2 t
    prettyPrec p (TExists tv t) = parPrec p 0 $ "∃" <> pretty tv <> dot <+> pretty t
    prettyPrec p (TRecurs tv t) = parPrec p 0 $ "μ" <> pretty tv <> dot <+> pretty t
    prettyPrec _ (TRow r) = brackets $ pretty r

instance PrettyPrec RowTy where
    pretty REmpty    = "ε"
    pretty (RVar tv) = pretty tv
    pretty (t :> r)  = pretty t <> ";" <+> pretty r

instance PrettyPrec Var where
    pretty (x, t) = pretty x <+> ":" <+> pretty t

instance PrettyPrec Val where
    prettyPrec _ (VLit l) = pretty l
    prettyPrec _ (VVar (x, _)) = pretty x
    prettyPrec _ (VFun (f, _)) = pretty f
    prettyPrec _ (VLabel l _) = "`" <> pretty l
    prettyPrec _ (VTuple vs) = brackets $ hsep $ punctuate ";" $ map pretty vs
    prettyPrec p (VPack t1 v t2) =
        parPrec p 0 $ hang 2 $ hsep ["pack", brackets (pretty t1 <> "," <+> pretty v) <> softline <> "as", prettyPrec 2 t2]
    prettyPrec p (VRoll v t) = parPrec p 0 $ hang 2 $ hsep ["roll", prettyMax v <> softline <> "as", prettyPrec 2 t]
    prettyPrec p (VUnroll v) = parPrec p 0 $ "unroll" <+> prettyPrec 1 v
    prettyPrec _ (VAnnot v t) = parens $ hang 2 $ sep [pretty v, ":" <+> pretty t]

instance PrettyPrec Bind where
    pretty (BVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (BCall x v1 vs2) = pretty x <+> "=" <> softline <> prettyMax v1
        <> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (BProj x v i) = pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty i
    pretty (BUnpack tv x v) = brackets (pretty tv <> "," <+> pretty x) <+> "=" <> softline <> "unpack" <+> prettyMax v

instance PrettyPrec Exp where
    pretty (ELet d e)    = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ECase v les) = vsep [ "case" <+> pretty v <+> "of"
                                , "  " <> align (vsep (map (\(li, ei) -> hang 2 $ sep [pretty li <+> "->", pretty ei]) les))]
    pretty (EReturn v)      = "ret" <+> prettyMax v
    pretty (EAnnot e t)  = parens $ hang 2 $ sep [pretty e, ":" <+> pretty t]

instance PrettyPrec Defn where
    pretty (Defn (f, _) xs e) = pretty f <+> hsep (map (parens . pretty) xs) <+> "=" <> line <> indent 2 (pretty e)

instance PrettyPrec Program where
    pretty (_, ds, e) = vsep (map pretty ds) <> line <> pretty e

class StripAnn a where
    stripAnnot :: a -> a

instance StripAnn Val where
    stripAnnot = cata $ \case
        VAnnotF v _ -> v
        v           -> embed v

instance StripAnn Bind where
    stripAnnot (BVal x v)       = BVal x (stripAnnot v)
    stripAnnot (BCall x v vs)   = BCall x (stripAnnot v) (map stripAnnot vs)
    stripAnnot (BProj x v i)    = BProj x (stripAnnot v) i
    stripAnnot (BUnpack tv x v) = BUnpack tv x (stripAnnot v)

instance StripAnn Exp where
    stripAnnot = cata $ \case
        EAnnotF e _ -> e
        e           -> embed e

instance StripAnn Defn where
    stripAnnot (Defn f xs e) = Defn f xs (stripAnnot e)

instance StripAnn Program where
    stripAnnot (decs, defs, e) = (decs, map stripAnnot defs, stripAnnot e)
