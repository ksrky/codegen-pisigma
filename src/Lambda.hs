{-# LANGUAGE TemplateHaskell #-}

module Lambda (
    Lit(..),
    Meta(..),
    Ty(..),
    TyF(..),
    Var,
    Exp(..),
    ExpF(..),
    Dec(..),
    Program,
    Ctx(..),
    varScope,
    labelScope,
    Env,
    lookupEnumEnv,
    lookupBindEnv,
    extendBindEnv,
    splitTFun,
    Typeable(..),
    stripAnnot
) where

import Id

import Control.Lens.Combinators
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.IORef
import GHC.Stack

newtype Lit = LInt Int
    deriving (Eq, Show)

newtype Meta = Meta (IORef (Maybe Ty))
    deriving (Eq)

instance Show Meta where
    show (Meta _) = "?m"

type Label = String

data Ty
    = TInt
    | TName Id
    | TFun Ty Ty
    | TTuple [Ty]
    | TMeta Meta
    deriving (Eq, Show)

type Var = (Id, Ty)

data Exp
    = ELit Lit
    | EVar Var
    | ELabel Label Ty
    | EApp Exp Exp
    | ELam Var Exp
    | EExtern Var [Exp]
    | ETuple [Exp]
    | ELet Var Exp Exp
    | ELetrec [(Var, Exp)] Exp
    | ECase Exp [(Label, Exp)]
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Dec
    = DEnum Id [Label]
    | DBind Id Ty
    deriving (Eq, Show)

type Program = ([Dec], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Exp

data Ctx = Ctx
    { _varScope   :: [(String, Var)]
    , _labelScope :: [(String, Ty)]
    }

makeLenses ''Ctx

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

splitTFun :: Ty -> ([Ty], Ty)
splitTFun = go []
  where
    go acc (TFun ty1 ty2) = go (ty1 : acc) ty2
    go acc ty             = (reverse acc, ty)

class Typeable a where
    typeof :: HasCallStack => a -> Ty

instance Typeable Lit where
    typeof _ = TInt

instance Typeable Ty where
    typeof = id

instance Typeable Var where
    typeof = snd

instance Typeable Exp where
    typeof = cata $ \case
        ELitF l -> typeof l
        EVarF v -> typeof v
        ELabelF _ t -> t
        EAppF t1 _ | TFun _ t12 <- t1 -> t12
                   | otherwise        -> error "impossible"
        ELamF (_, t1) t2 -> TFun t1 t2
        EExternF (_, t) _ -> snd $ splitTFun t
        ELetF _ _ t -> t
        ELetrecF _ t -> t
        ETupleF ts -> TTuple ts
        ECaseF _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EAnnotF _ t -> t

stripAnnot :: Exp -> Exp
stripAnnot = cata $ \case
    EAnnotF e _ -> e
    e -> embed e
