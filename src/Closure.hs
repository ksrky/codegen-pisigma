{-# LANGUAGE TemplateHaskell #-}

module Closure (
    Lit(..),
    Ty(..),
    TyF(..),
    Row(..),
    Var,
    Val(..),
    ValF(..),
    Bind(..),
    Exp(..),
    ExpF(..),
    Dec(..),
    Def(..),
    Prog,
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
    mkTTuple) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import GHC.Stack
import Id
import Prettyprinter            hiding (Pretty (..))
import Prettyprinter.Prec

newtype Lit = LInt Int
    deriving (Eq, Show)

type Label = String

data Ty
    = TInt
    | TVar Int
    | TName Id
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
    | VLab Label Ty
    | VTuple [Val]
    | VPack Ty Val Ty
    | VRoll Val Ty
    | VUnroll Val
    | VValTy Val Ty
    deriving (Eq, Show)

data Bind
    = BVal Var Val
    | BCall Var Val [Val]
    | BProj Var Val Int
    | BUnpack Var Val -- ignoring the type variable
    deriving (Eq, Show)

data Exp
    = ELet Bind Exp
    | ECase Val [(Label, Exp)]
    | ERet Val
    | EExpTy Exp Ty
    deriving (Eq, Show)

data Dec
    = DEnum Id [Label]
    | DBind Id Ty
    deriving (Eq, Show)

data Def = Def {code :: Var, args :: [Var], body :: Exp}
    deriving (Eq, Show)

type Prog = ([Dec], [Def], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Row
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
mkClos ts1 t2 = TEx $ TRec $ TRow $ TFun (TVar 0 : ts1) t2 :> RVar 1

-- | Make an unpacked closure
mkUClos :: [Ty] -> Ty -> Row -> Ty
mkUClos ts1 t2 r = TRec $ TRow (TFun (TVar 0 : ts1) t2 :> r)

mkTTuple :: [Ty] -> Ty
mkTTuple ts = TRow $ foldr (:>) REmpty ts

unrollUClos :: Ty -> Ty -> Ty
unrollUClos s (TRow (TFun (TVar 0: ts1) t2 :> r)) = TRow $ TFun (s : ts1) t2 :> r
unrollUClos _ _                                   = error "impossible"

unpackClos :: Ty -> Ty -> Ty
unpackClos (TRow r) (TRec (TRow (TFun ts1 t2 :> RVar 1))) = TRec $ TRow $ TFun ts1 t2 :> r
unpackClos  _ _                                           = error "impossible"

bindVar :: Bind -> Var
bindVar (BVal x _)    = x
bindVar (BCall x _ _) = x
bindVar (BProj x _ _) = x
bindVar (BUnpack x _) = x

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
        VLabF _ t -> t
        VTupleF vs -> TRow $ foldr ((:>) . typeof) REmpty vs
        VPackF _ _ t -> t
        VRollF _ t -> t
        VUnrollF v ->
            case typeof v of
                TRec t -> unrollUClos (TRec t) t
                _      -> error "required recursive type"
        VValTyF _ t -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELetF _ t -> t
        ECaseF _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        ERetF v -> typeof v
        EExpTyF _ t -> t

instance PrettyPrec Lit where
    pretty (LInt n) = pretty n

instance PrettyPrec Ty where
    prettyPrec _ TInt = "Int"
    prettyPrec _ (TVar tv) = pretty tv
    prettyPrec _ (TName l) = pretty l
    prettyPrec p (TFun ts t) = parPrec p 2 $
        parens (hsep $ punctuate "," $ map (prettyPrec 1) ts) <+> "->" <+> prettyPrec 2 t
    prettyPrec p (TEx t) = parPrec p 0 $ "∃." <+> pretty t
    prettyPrec p (TRec t) = parPrec p 0 $ "μ." <+> pretty t
    prettyPrec _ (TRow r) = brackets $ pretty r

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
    prettyPrec _ (VLab l _) = "`" <> pretty l
    prettyPrec _ (VTuple vs) = brackets $ hsep $ punctuate ";" $ map pretty vs
    prettyPrec p (VPack t1 v t2) =
        parPrec p 0 $ hang 2 $ hsep ["pack", brackets (pretty t1 <> "," <+> pretty v) <> softline <> "as", prettyPrec 2 t2]
    prettyPrec p (VRoll v t) = parPrec p 0 $ hang 2 $ hsep ["roll", prettyMax v <> softline <> "as", prettyPrec 2 t]
    prettyPrec p (VUnroll v) = parPrec p 0 $ "unroll" <+> prettyPrec 1 v
    prettyPrec _ (VValTy v t) = parens $ hang 2 $ sep [pretty v, ":" <+> pretty t]

instance PrettyPrec Bind where
    pretty (BVal x v) = pretty x <+> "=" <> softline <> pretty v
    pretty (BCall x v1 vs2) = pretty x <+> "=" <> softline <> prettyMax v1
        <> parens (hsep (punctuate "," (map prettyMax vs2)))
    pretty (BProj x v i) = pretty x <+> "=" <> softline <> prettyMax v <> "." <> pretty i
    pretty (BUnpack x v) = brackets ("_," <+> pretty x) <+> "=" <> softline <> "unpack" <+> prettyMax v

instance PrettyPrec Exp where
    pretty (ELet d e)    = vsep [hang 2 ("let" <+> pretty d) <+> "in", pretty e]
    pretty (ECase v les) = vsep [ "case" <+> pretty v <+> "of"
                                , "  " <> align (vsep (map (\(li, ei) -> hang 2 $ sep [pretty li <+> "->", pretty ei]) les))]
    pretty (ERet v)      = "ret" <+> prettyMax v
    pretty (EExpTy e t)  = parens $ hang 2 $ sep [pretty e, ":" <+> pretty t]

instance PrettyPrec Def where
    pretty (Def (f, _) xs e) = pretty f <+> hsep (map (parens . pretty) xs) <+> "=" <> line <> indent 2 (pretty e)

instance PrettyPrec Prog where
    pretty (_, ds, e) = vsep (map pretty ds) <> line <> pretty e

class StripAnn a where
    stripAnn :: a -> a

instance StripAnn Val where
    stripAnn = cata $ \case
        VValTyF v _ -> v
        v           -> embed v

instance StripAnn Bind where
    stripAnn (BVal x v)     = BVal x (stripAnn v)
    stripAnn (BCall x v vs) = BCall x (stripAnn v) (map stripAnn vs)
    stripAnn (BProj x v i)  = BProj x (stripAnn v) i
    stripAnn (BUnpack x v)  = BUnpack x (stripAnn v)

instance StripAnn Exp where
    stripAnn = cata $ \case
        EExpTyF e _ -> e
        e           -> embed e

instance StripAnn Def where
    stripAnn (Def f xs e) = Def f xs (stripAnn e)

instance StripAnn Prog where
    stripAnn (decs, defs, e) = (decs, map stripAnn defs, stripAnn e)
