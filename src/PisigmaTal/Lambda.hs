{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module PisigmaTal.Lambda (
    Lit(..),
    Meta(..),
    Ty(..),
    TyF(..),
    Var,
    Op(..),
    Exp(..),
    ExpF(..),
    Bind(..),
    BindBlock(..),
    pattern NonrecBind,
    pattern MutrecBinds,
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

import Control.Lens.Combinators hiding (op)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.IORef
import GHC.Stack
import PisigmaTal.Id
import PisigmaTal.Primitive
import Prelude                  hiding (Enum)

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

data Op
    = KnownOp Id Ty
    | PrimOp PrimOp Ty
    deriving (Eq, Show)

data Exp
    = ELit Lit
    | EVar Var
    | ELabel EnumId Label
    | EApp Exp Exp
    | EFullApp Op [Exp]
    | ELam Var Exp
    | ETuple [Exp]
    | ELet BindBlock Exp
    | ECase EnumId Exp [(Label, Exp)]
    | EAnnot Exp Ty
    deriving (Eq, Show)

data Bind = Bind Var Exp
    deriving (Eq, Show)

data BindBlock
    = Nonrec Bind
    | Mutrec [Bind]
    deriving (Eq, Show)

pattern NonrecBind :: Var -> Exp -> BindBlock
pattern NonrecBind x e = Nonrec (Bind x e)

pattern MutrecBinds :: [(Var, Exp)] -> BindBlock
pattern MutrecBinds xes <- Mutrec (map (\(Bind x e) -> (x, e)) -> xes) where
    MutrecBinds xes = Mutrec (map (uncurry Bind) xes)

{-# COMPLETE NonrecBind, MutrecBinds #-}

type EnumId = Id

data Dec
    = DEnum EnumId [Label]
    | DBind Id Ty
    deriving (Eq, Show)

type Program = ([Dec], Exp)

makeBaseFunctor ''Ty
makeBaseFunctor ''Exp

data Ctx = Ctx
    { _varScope   :: [(String, Var)]
    , _labelScope :: [(String, EnumId)]
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

instance Typeable Op where
    typeof = \case
        KnownOp _ t -> t
        PrimOp _ t  -> t

instance Typeable Exp where
    typeof = cata $ \case
        ELitF l -> typeof l
        EVarF v -> typeof v
        ELabelF c _ -> TName c
        EAppF t1 _ | TFun _ t12 <- t1 -> t12
                   | otherwise        -> error "impossible"
        EFullAppF op _ -> snd $ splitTFun $ typeof op
        ELamF (_, t1) t2 -> TFun t1 t2
        ELetF _ t -> t
        ETupleF ts -> TTuple ts
        ECaseF _ _ lts | (_, t) : _ <- lts -> t
                     | otherwise         -> error "impossible"
        EAnnotF _ t -> t

stripAnnot :: Exp -> Exp
stripAnnot = cata $ \case
    EAnnotF e _ -> e
    e -> embed e
